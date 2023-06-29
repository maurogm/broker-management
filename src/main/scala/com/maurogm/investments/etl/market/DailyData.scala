package com.maurogm.investments.etl.market

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.{CSVParser, CSVSerializer, CurrencyHomogenizer, DateTimeAccessor}

import java.time.LocalDateTime

case class DailyData(
    datetime: LocalDateTime,
    currency: String,
    lastPrice: Money,
    openPrice: Money,
    maxPrice: Money,
    minPrice: Money,
    montoOperado: Option[Money],
    volumenNominal: Long,
    cantidadOperaciones: Option[Long]
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
      val tokens = str.split(",", -1)
      val currency = tokens(1)
      def validateString(str: String): Option[String] = if (str.isEmpty) Option.empty else Option(str)
      DailyData(
        LocalDateTime.parse(tokens(0)),
        currency,
        Money(currency, BigDecimal(tokens(2))),
        Money(currency, BigDecimal(tokens(3))),
        Money(currency, BigDecimal(tokens(4))),
        Money(currency, BigDecimal(tokens(5))),
        validateString(tokens(6)).map(str => Money(currency, BigDecimal(str))),
        tokens(7).toLong,
        validateString(tokens(8)).map(_.toLong)
      )
    }
  }

  given currencyHomogenizerDailyData(using cc: CurrencyConverter): CurrencyHomogenizer[DailyData] with {
    def homogenizeCurrency(x: DailyData): DailyData = x.copy(
      lastPrice = x.lastPrice.convert(x.datetime.toLocalDate),
      openPrice = x.openPrice.convert(x.datetime.toLocalDate),
      maxPrice = x.maxPrice.convert(x.datetime.toLocalDate),
      minPrice = x.minPrice.convert(x.datetime.toLocalDate),
      montoOperado = x.montoOperado.map(_.convert(x.datetime.toLocalDate)),
    )
  }
}
