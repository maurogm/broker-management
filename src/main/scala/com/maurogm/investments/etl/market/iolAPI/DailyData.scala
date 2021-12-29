package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Money

import java.time.LocalDateTime

case class DailyData(
    fechaHora: LocalDateTime,
    currency: String,
    closePrice: Money,
    openPrice: Money,
    maxPrice: Money,
    minPrice: Money,
    montoOperado: Money,
    volumenNominal: Long,
    cantidadOperaciones: Long
)

object DailyData {
  given orderingDailyData: Ordering[DailyData] with {
    override def compare(x: DailyData, y: DailyData): Int = {
      x.fechaHora.toString.compare(y.fechaHora.toString)
    }
  }
}
