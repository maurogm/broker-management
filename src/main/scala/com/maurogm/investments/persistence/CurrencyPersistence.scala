package com.maurogm.investments.persistence

import com.maurogm.investments.currency.CurrencyConverter
import com.maurogm.investments.etl.util.CSVSerializer
import com.maurogm.investments.etl.util.Utils.writeAsCsv

import java.time.LocalDate

object CurrencyConversionPersistence {
  def persistAsCSV(
      cc: CurrencyConverter,
      filePathsRoot: String = "src/main/resources/outputs/currencies/",
      fileName: Option[String] = None
  ): Unit = {
    val headers = Seq("currency_from", "currency_to", "date", "rate")

    val newCurrency = cc.symbol
    println(newCurrency)
    val filePath = filePathsRoot + fileName.getOrElse(s"$newCurrency.csv")
    println(filePath)

    val dateDatapoints = cc.table.map { case ((currencyFrom, date), rate) =>
      CurrencyDatapoint(currencyFrom, newCurrency, date, rate)
    }.toSeq

    writeAsCsv(
      data = dateDatapoints,
      filePath = filePath,
      append = false,
      headers = Option(headers)
    )
  }
}

case class CurrencyDatapoint(
    currencyFrom: String,
    currencyTo: String,
    date: LocalDate,
    rate: BigDecimal
) extends CSVSerializer {
  override def toCsv: String = s"$currencyFrom,$currencyTo,$date,$rate"
}
