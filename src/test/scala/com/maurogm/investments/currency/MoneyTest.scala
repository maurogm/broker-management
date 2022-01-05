package com.maurogm.investments.currency

import com.maurogm.investments.currency.Money
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

class MoneyTest extends AnyFreeSpec with Matchers {

  val ars: Money = Money("ARS", 100)
  val usd: Money = Money("USD", 20)
  val moreArs: Money = Money("ARS", 5)

  "Money" - {
    "when trying to combine with Money in a different currency" - {
      "should throw an exception" in {
        assertThrows[IllegalArgumentException](ars + usd)
        assertThrows[IllegalArgumentException](ars - usd)
        assertThrows[IllegalArgumentException](ars * usd)
      }
    }

    "when attempting legal operations" - {
      "should be summed correctly" in {
        ars + moreArs shouldBe Money("ARS", 105)
      }
      "should be subtracted correctly" in {
        ars - moreArs shouldBe Money("ARS", 95)
      }
      "should be multiplied correctly against other money" in {
        ars * moreArs shouldBe Money("ARS", 500)
      }
      "should be multiplied correctly against a scalar" in {
        ars * 2 shouldBe Money("ARS", 200)
      }
    }

    "when converting to another currency" - {
      "should ok" in {
        ars.convertTo("FOO", 0.05) shouldBe Money("FOO", 5)
        ars.convertTo("BAR", 2) shouldBe Money("BAR", 200)
      }
      "should not be allowed to convert to itself unless exchRate == 1" in {
        ars.convertTo("ARS", 1) shouldBe ars
        assertThrows[IllegalArgumentException](ars.convertTo("ARS", 2))
      }
    }
  }

  "An Ordering[Money]" - {
    "should be available to sort a List[Money]" in {
      List(ars, moreArs).sorted shouldBe List(moreArs, ars)
    }
  }

}
