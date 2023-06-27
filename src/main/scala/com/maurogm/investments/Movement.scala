package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.{CSVSerializer, CurrencyHomogenizer}

import java.time.LocalDate

case class Movement(
    broker: String,
    date: LocalDate,
    movementType: MovementType,
    associatedTicker: Option[String],
    amount: Money
) extends CSVSerializer {
  override def toCsv: String = {
    this.toString
      .replace("Movement(", "")
      .replace(s"Money(", "")
      .replace("Some(", "")
      .replace(")", "")
      .replace("None", "")
  }
}

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
  case CashDeposit, CashWithdrawal, CashConversion, Dividends, Amortization, Credit, Costs, AssetTransferIn, AssetTransferOut, Operation
}
