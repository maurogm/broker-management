package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Money
import com.maurogm.investments.etl.util.{
  CSVParser,
  CSVSerializer,
  DateTimeAccessor
}

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
) extends CSVSerializer
    with DateTimeAccessor[DailyData] {

  override def toCsv: String = this.toString
    .replace("DailyData(", "")
    .replace(s"Money($currency,", "")
    .replace(")", "")

  override def getDateTime: LocalDateTime = fechaHora

  override def setDateTime(newDateTime: LocalDateTime): DailyData =
    this.copy(fechaHora = newDateTime)
}

object DailyData {
  given orderingDailyData: Ordering[DailyData] with {
    override def compare(x: DailyData, y: DailyData): Int = {
      x.fechaHora.toString.compare(y.fechaHora.toString)
    }
  }

  given csvParserDailyData: CSVParser[DailyData] = new CSVParser[DailyData] {
    override def fromCsv(str: String): DailyData = {
      val tokens = str.split(",")
      val currency = tokens(1)
      DailyData(
        LocalDateTime.parse(tokens(0)),
        currency,
        Money(currency, BigDecimal(tokens(2))),
        Money(currency, BigDecimal(tokens(3))),
        Money(currency, BigDecimal(tokens(4))),
        Money(currency, BigDecimal(tokens(5))),
        Money(currency, BigDecimal(tokens(6))),
        tokens(7).toLong,
        tokens(8).toLong
      )
    }
  }
}
