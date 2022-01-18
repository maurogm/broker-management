package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.CurrencyHomogenizer

import java.time.LocalDateTime

case class Order(
    broker: String,
    datetime: LocalDateTime,
    asset: Asset,
    operationType: String,
    quantity: Double,
    price: Money,
    costs: Money,
    total: Money
)

object Order {
  given orderingOfOrder: Ordering[Order] with {
    override def compare(x: Order, y: Order): Int = {
      x.datetime.toString.compare(y.datetime.toString)
    }
  }

  given currencyHomogenizerOrder(using
      cc: CurrencyConverter
  ): CurrencyHomogenizer[Order] with {
    def homogenizeCurrency(x: Order): Order = x.copy(
      price = x.price.convert(x.datetime.toLocalDate),
      costs = x.costs.convert(x.datetime.toLocalDate),
      total = x.total.convert(x.datetime.toLocalDate)
    )
  }
}
