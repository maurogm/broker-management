package com.maurogm.investments

import java.time.LocalDateTime

case class Order(
    broker: String,
    datetime: LocalDateTime,
    exchange: String,
    ticker: String,
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
}
