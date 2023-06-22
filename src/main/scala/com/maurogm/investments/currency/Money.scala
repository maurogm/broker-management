package com.maurogm.investments.currency

import java.time.LocalDate

// TODO: Reemplazar mi clase Money por https://github.com/typelevel/squants/tree/master/shared/src/main/scala/squants/market

case class Money(currency: String, amount: BigDecimal) {
  def checkSameCurrency(that: Money): Unit = require(
    this.currency == that.currency,
    s"Can't operate between ${this} and ${that} since they have different currencies"
  )

  infix def +(that: Money): Money = {
    checkSameCurrency(that)
    Money(currency, this.amount + that.amount)
  }

  infix def -(that: Money): Money = {
    checkSameCurrency(that)
    Money(currency, this.amount - that.amount)
  }

  infix def *(that: Money): Money = {
    checkSameCurrency(that)
    Money(currency, this.amount * that.amount)
  }

  infix def *(x: BigDecimal): Money = {
    Money(currency, this.amount * x)
  }

  infix def /(that: Money): BigDecimal = {
    checkSameCurrency(that)
    this.amount / that.amount
  }

  /**
   * @param exchRate is the rate of oldCurr / newCurr
   */
  def convertTo(newCurr: String, exchRate: BigDecimal): Money = {
    require(newCurr != currency || exchRate == 1)
    Money(newCurr, amount / exchRate)
  }

  def convert(date: LocalDate)(using cc: CurrencyConverter): Money = {
    cc.convert(this, date)
  }
}

object Money {
  given orderingOfMoney: Ordering[Money] with {
    override def compare(x: Money, y: Money): Int = {
      x.checkSameCurrency(y)
      x.amount.compare(y.amount)
    }
  }

}
