package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
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
import scala.util.{Failure, Success, Try}

object HistoricalData {

  /** Makes a request to IOL API's to get the historical data of a certain
    * active.
    *
    * @param exchange
    *   The exchange in which `ticker` is traded. One of `bCBA`, `nYSE`,
    *   `nASDAQ`, `aMEX`, `bCS`, `rOFX`.
    */
  def getHistoricalData(
      asset: Asset,
      dateStart: LocalDate,
      dateEnd: LocalDate,
      adjust: Boolean = false
  )(using authToken: AuthenticationToken): Seq[DailyData] = {
    if (dateStart isAfter dateEnd) Seq()
    else {
      val url =
        URLs.historicalDataUrl(
          asset.exchange,
          asset.ticker,
          dateStart,
          dateEnd,
          adjust
        )
      makeRequest(url).toJson
        .as[Seq[Map[String, JsValue]]]
        .map(parseHistoricalData(_).toDailyData)
    }
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

  private def localFilepath(asset: Asset): String =
    s"src/main/resources/market/${asset.exchange.toUpperCase}/${asset.ticker.toUpperCase}.csv"

  private def writeHistory(
      data: Seq[DailyData],
      asset: Asset,
      append: Boolean = false
  ): Unit = {

    writeAsCsv(data, localFilepath(asset), append)
  }

  def readHistoryFromCsv(asset: Asset): Seq[DailyData] = {
    readFromCSV[DailyData](localFilepath(asset)) match {
      case Success(history) => history
      case Failure(_) =>
        throw new NoSuchElementException(s"Could not find history for $asset")
    }
  }

  private def fillMissingDailyData(data: Seq[DailyData]): Seq[DailyData] =
    consecutiveMap(data).flatMap(consecutiveFillOfDates)

  def updateHistory(
      asset: Asset,
      dateStart: LocalDate,
      dateEnd: LocalDate,
      adjust: Boolean = false
  )(using authToken: AuthenticationToken): Unit = {
    def getData(start: LocalDate, end: LocalDate) =
      getHistoricalData(asset, start, end, adjust)
    val maybeCurrentHistory = Try(readHistoryFromCsv(asset))
    val newHistory =
      if (maybeCurrentHistory.isFailure || maybeCurrentHistory.get.isEmpty)
        getData(dateStart, dateEnd)
      else {
        val currentHistory = maybeCurrentHistory.get
        val minDateTime: LocalDateTime = currentHistory.min.getDateTime
        val maxDateTime: LocalDateTime = currentHistory.max.getDateTime
        lazy val previousHistory =
          getData(dateStart, minDateTime.toLocalDate.minusDays(1))
        lazy val posteriorHistory = getData(maxDateTime.toLocalDate, dateEnd)
        previousHistory ++ currentHistory.filter(
          _.getDateTime.toLocalDate isBefore maxDateTime.toLocalDate
        ) ++ posteriorHistory
      }
    writeHistory(fillMissingDailyData(newHistory), asset, false)
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
