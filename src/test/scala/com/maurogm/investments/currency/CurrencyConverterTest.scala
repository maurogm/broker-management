package com.maurogm.investments.currency

import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers

import java.time.LocalDate

class CurrencyConverterTest  extends AnyFreeSpec with Matchers {

  val date: LocalDate = LocalDate.parse("2020-01-01")
  val table = Map(("ARS", date) -> BigDecimal(200))
  val converter = new CurrencyConverter("USD", table)
  val arsMoney: Money = Money("ARS", BigDecimal(1000))
  val usdMoney: Money = Money("USD", BigDecimal(5))

  val dateNotInScope: LocalDate = LocalDate.parse("2020-01-02")

  "convert" - {
    "when applied to a date and currency in the table" - {
      "should convert correctly" in {
        converter.convert(arsMoney, date) shouldBe usdMoney
      }
    }

    "when applied to a date not in the table" - {
      "should throw an exception" in {
        assertThrows[NoSuchElementException](converter.convert(arsMoney, dateNotInScope))
      }
    }

    "when applied to a currency not in the table" - {
      "should throw an exception" in {
        assertThrows[NoSuchElementException](converter.convert(Money("XXX", BigDecimal(0)), date))
      }
    }
  }
}
