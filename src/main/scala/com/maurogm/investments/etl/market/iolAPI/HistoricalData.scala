package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Money
import com.maurogm.investments.etl.market.iolAPI.URLs.makeRequest
import com.maurogm.investments.etl.market.iolAPI.{
  AuthenticationToken,
  DailyData,
  URLs
}
import com.maurogm.investments.etl.util.DateGapFiller.{
  consecutiveFillOfDates,
  consecutiveMap
}
import com.maurogm.investments.etl.util.Utils.ResponseExtensions.toJson
import com.maurogm.investments.etl.util.Utils.{readFromCSV, writeAsCsv}
import com.maurogm.investments.etl.util.{CSVParser, CSVSerializer}
import play.api.libs.json.{JsValue, Json}
import requests.Response

import java.io.{BufferedWriter, File, FileWriter}
import java.time.temporal.ChronoUnit
import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

object HistoricalData {

  /** Makes a request to IOL API's to get the historical data of a certain
    * active.
    *
    * @param exchange
    *   The exchange in which `ticker` is traded. One of `bCBA`, `nYSE`,
    *   `nASDAQ`, `aMEX`, `bCS`, `rOFX`.
    */
  def getHistoricalData(
      exchange: String,
      ticker: String,
      dateStart: LocalDate | String,
      dateEnd: LocalDate | String,
      adjust: Boolean = false
  )(using authToken: AuthenticationToken): Seq[DailyData] = {
    val url =
      URLs.historicalDataUrl(exchange, ticker, dateStart, dateEnd, adjust)
    makeRequest(url).toJson
      .as[Seq[Map[String, JsValue]]]
      .map(parseHistoricalData(_).toDailyData)
  }

  private def parseHistoricalData(jsonMap: Map[String, JsValue]): Cotizacion = {
    val moneda = jsonMap("moneda").as[String]
    require(
      Set("dolar_Estadounidense", "peso_Argentino").contains(moneda),
      s"Could not parse moneda=$moneda as USD nor ARS"
    )
    val currency = if moneda == "peso_Argentino" then "ARS" else "USD"

    val dateTime = LocalDateTime.parse(jsonMap("fechaHora").as[String])

    def getAsMoney(key: String): Money =
      Money(currency, jsonMap(key).as[BigDecimal])
    def getIfNotNull(key: String) =
      jsonMap
        .get(key)
        .filter(value => value != null && value.toString != "null")
    val parsedPuntas =
      getIfNotNull("puntas").map(_.as[Seq[Map[String, Double]]])

    Cotizacion(
      getAsMoney("ultimoPrecio"),
      jsonMap("variacion").as[Double],
      getAsMoney("apertura"),
      getAsMoney("maximo"),
      getAsMoney("minimo"),
      dateTime,
      jsonMap("tendencia").as[String],
      getAsMoney("cierreAnterior"),
      getAsMoney("montoOperado"),
      jsonMap("volumenNominal").as[Long],
      getAsMoney("precioPromedio"),
      currency,
      getAsMoney("precioAjuste"),
      jsonMap("interesesAbiertos").as[Double],
      parsedPuntas,
      jsonMap("cantidadOperaciones").as[Long],
      getIfNotNull("descripcionTitulo").map(_.as[String]),
      getIfNotNull("plazo").map(_.as[String]),
      jsonMap("laminaMinima").as[Double],
      jsonMap("lote").as[Double]
    )
  }

  private def localFilepath(exchange: String, ticker: String): String =
    s"src/main/resources/market/${exchange.toUpperCase}/${ticker.toUpperCase}.csv"

  private def writeHistoryData(
      data: Seq[DailyData],
      exchange: String,
      ticker: String,
      append: Boolean = true
  ): Unit = {

    writeAsCsv(data, localFilepath(exchange, ticker), false)
  }

  def readHistoryFromCsv[T](
      exchange: String,
      ticker: String
  ): Seq[DailyData] = {
    readFromCSV[DailyData](localFilepath(exchange, ticker))
  }

  def main(args: Array[String]): Unit = {
    given authToken: AuthenticationToken = new AuthenticationToken

    val bearerToken = authToken.getAccessToken
    println(bearerToken)

    val exchange = "BCBA"
    val ticker = "GD30C"
    val dateStart = "2021-12-15"
    // val dateEnd = "2021-12-21"
    val dateEnd = "2021-12-28"

    val history =
      getHistoricalData(exchange, ticker, dateStart, dateEnd, false)

    println(history.sorted.mkString("\n"))

    println("consecutiveMap:")
    val consecutives = consecutiveMap(history)
    println(
      consecutives
        .map { case (a, b) => (a.fechaHora, b.fechaHora) }
        .mkString("\n")
    )
    println("consecutiveFill:")
    val filled = consecutives.flatMap(consecutiveFillOfDates)
    println(filled.map(_.toCsv).mkString("\n"))
    println(filled.map { _.getDateTime }.mkString("\n"))

    writeHistoryData(
      consecutiveMap(history).flatMap(consecutiveFillOfDates),
      exchange,
      ticker,
      false
    )

    println("READ:")
    val read = readHistoryFromCsv("bcba", "AL30")
    read.foreach(println)
  }
}

/** Case class with the parsed fields of ALL the fields gotten in the Response
  */
case class Cotizacion(
    ultimoPrecio: Money,
    variacion: Double,
    apertura: Money,
    maximo: Money,
    minimo: Money,
    fechaHora: LocalDateTime,
    tendencia: String,
    cierreAnterior: Money,
    montoOperado: Money,
    volumenNominal: Long,
    precioPromedio: Money,
    currency: String,
    precioAjuste: Money,
    interesesAbiertos: Double,
    puntas: Option[Seq[Map[String, Double]]],
    cantidadOperaciones: Long,
    descripcionTitulo: Option[String],
    plazo: Option[String],
    laminaMinima: Double,
    lote: Double
) {
  def toDailyData: DailyData = DailyData(
    fechaHora,
    currency,
    ultimoPrecio,
    apertura,
    maximo,
    minimo,
    montoOperado,
    volumenNominal,
    cantidadOperaciones
  )
}
