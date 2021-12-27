package com.maurogm.brokers

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
}

enum MovementType {
  case CashDeposit, CashWithdrawal, Dividends, Amortization, Credit, Costs
}
