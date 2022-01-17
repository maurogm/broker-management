package com.maurogm.investments

import com.maurogm.investments.currency.Money
import com.maurogm.investments.{Position, PositionFraction}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

import java.time.{LocalDate, LocalDateTime}

object Generators {
  val quantityGen: Gen[Int] = Gen.choose(1, 10000)
  val usdGen: Gen[Money] = for {
    amount <- Gen.choose[Double](1, 100)
  } yield Money("USD", amount)
  val datetimeGen: Gen[LocalDateTime] = Gen.choose[LocalDateTime](
    LocalDate.parse("2000-01-01").atStartOfDay(),
    LocalDate.parse("2020-01-01").atStartOfDay()
  )

  val positionFractionGen: Gen[PositionFraction] = for {
    datetimeOpening <- datetimeGen
    pricePayed <- usdGen
    remainingQuantity <- quantityGen
  } yield PositionFraction(datetimeOpening, pricePayed, remainingQuantity)

  val positionGen: Gen[Position] = for {
    breakdown <- Gen.containerOf[List, PositionFraction](positionFractionGen)
  } yield Position(breakdown)
}

class PositionTest
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with Matchers {

  implicit override val generatorDrivenConfig =
    PropertyCheckConfiguration(
      minSuccessful = 50,
      maxDiscardedFactor = 10,
      sizeRange = 20
    )

  property(
    "total should summarize the sum of the Position Fraction quantities"
  ) {
    forAll(Generators.positionGen) { (position: Position) =>
      position.breakdown
        .map(_.remainingQuantity)
        .fold(BigDecimal(0))(_ + _) shouldBe position.total
    }
  }

  property(
    "buy: After buying, the new position's breakdown should contain the PositionFraction just bought"
  ) {
    forAll(
      Generators.positionGen,
      Generators.quantityGen,
      Generators.usdGen,
      Generators.datetimeGen
    ) {
      (
          position: Position,
          quantity: Int,
          price: Money,
          datetime: LocalDateTime
      ) =>
        val newPosition = position.buy(quantity, price, datetime)
        val boughtPositionFraction = PositionFraction(datetime, price, quantity)
        newPosition.breakdown should contain(boughtPositionFraction)
    }
  }

  property(
    "buy: After buying, the position's total should increase by the bought quantity"
  ) {
    forAll(
      Generators.positionGen,
      Generators.quantityGen,
      Generators.usdGen,
      Generators.datetimeGen
    ) {
      (
          position: Position,
          quantity: Int,
          price: Money,
          datetime: LocalDateTime
      ) =>
        val newPosition = position.buy(quantity, price, datetime)
        newPosition.total shouldBe position.total + quantity
    }
  }

  property(
    "sell: After selling, the position's total should decrease by the sold quantity"
  ) {
    forAll(
      Generators.positionGen,
      Generators.quantityGen,
      Generators.usdGen,
      Generators.datetimeGen
    ) {
      (
          position: Position,
          quantity: Int,
          price: Money,
          datetime: LocalDateTime
      ) =>
        whenever(quantity <= position.total) {
          val newPosition = position.sell(quantity, price, datetime)._1
          position.total - quantity shouldBe newPosition.total +- 0.000000001
        }
    }
  }

  property("sell: Attempts to sell more than is owned should fail") {
    forAll(
      Generators.positionGen,
      Gen.choose(10000, 1000000),
      Generators.usdGen,
      Generators.datetimeGen
    ) {
      (
          position: Position,
          quantity: Int,
          price: Money,
          datetime: LocalDateTime
      ) =>
        whenever(quantity > position.total) {
          an[IllegalArgumentException] should be thrownBy position.sell(
            quantity,
            price,
            datetime
          )
          // assertThrows[IllegalArgumentException](position.sell(quantity, price, datetime))
        }
    }
  }
  property("sell")(pending)
}
