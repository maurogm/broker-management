package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.{
  CSVParser,
  CSVSerializer,
  CurrencyHomogenizer,
  DateTimeAccessor
}

import java.time.LocalDateTime

case class DailyData(
    datetime: LocalDateTime,
    currency: String,
    lastPrice: Money,
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

  override def getDateTime: LocalDateTime = datetime

  override def setDateTime(newDateTime: LocalDateTime): DailyData =
    this.copy(datetime = newDateTime)
}

object DailyData {
  given orderingDailyData: Ordering[DailyData] with {
    override def compare(x: DailyData, y: DailyData): Int = {
      x.datetime.toString.compare(y.datetime.toString)
    }
  }

  given csvParserDailyData: CSVParser[DailyData] with {
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

  given currencyHomogenizerDailyData(using cc: CurrencyConverter): CurrencyHomogenizer[DailyData] with {
    def homogenizeCurrency(x: DailyData): DailyData = x.copy(
      lastPrice = x.lastPrice.convert(x.datetime.toLocalDate),
      openPrice = x.openPrice.convert(x.datetime.toLocalDate),
      maxPrice = x.maxPrice.convert(x.datetime.toLocalDate),
      minPrice = x.minPrice.convert(x.datetime.toLocalDate),
      montoOperado = x.montoOperado.convert(x.datetime.toLocalDate),
    )
  }
}
