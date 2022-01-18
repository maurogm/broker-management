package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.CurrencyHomogenizer

import java.time.LocalDate

case class Movement(
    broker: String,
    date: LocalDate,
    movementType: MovementType,
    associatedTicker: Option[String],
    amount: Money
)

object Movement {
  given orderingOfMovement: Ordering[Movement] with {
    override def compare(x: Movement, y: Movement): Int = {
      x.date.toString.compare(y.date.toString)
    }
  }

  given currencyHomogenizerMovement(using
      cc: CurrencyConverter
  ): CurrencyHomogenizer[Movement] with {
    def homogenizeCurrency(x: Movement): Movement = x.copy(
      amount = x.amount.convert(x.date)
    )
  }
}

enum MovementType {
  case CashDeposit, CashWithdrawal, Dividends, Amortization, Credit, Costs
}
