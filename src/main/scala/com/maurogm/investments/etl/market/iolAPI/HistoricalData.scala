package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.market.AssetExtension.{
  localFilepath,
  readHistoryFromCsv
}
import com.maurogm.investments.etl.market.iolAPI.URLs.makeRequest
import com.maurogm.investments.etl.market.iolAPI.{AuthenticationToken, URLs}
import com.maurogm.investments.etl.market.{AssetExtension, DailyData}
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
  )(using authToken: AuthenticationToken): Try[Seq[DailyData]] = {
    if (dateStart isAfter dateEnd) Success(Seq())
    else {
      val url =
        URLs.historicalDataUrl(
          asset.exchange,
          asset.ticker,
          dateStart,
          dateEnd,
          adjust
        )
      makeRequest(url).map(response =>
        response.toJson
        .as[Seq[Map[String, JsValue]]]
        .map(parseHistoricalData(_).toDailyData))
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

  private def writeHistory(
      data: Seq[DailyData],
      asset: Asset,
      append: Boolean = false
  ): Unit = {
    writeAsCsv(data, asset.localFilepath, append)
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
    val maybeCurrentHistory = readHistoryFromCsv(asset)
    val tryNewHistory =
      if (maybeCurrentHistory.isLeft || maybeCurrentHistory.getOrElse(List.empty).isEmpty)
        getData(dateStart, dateEnd)
      else {
        val currentHistory = maybeCurrentHistory.getOrElse(List.empty)
        val minDateTime: LocalDateTime = currentHistory.min.getDateTime
        val maxDateTime: LocalDateTime = currentHistory.max.getDateTime
        lazy val tryPreviousHistory =
          getData(dateStart, minDateTime.toLocalDate.minusDays(1))
        lazy val tryPosteriorHistory = getData(maxDateTime.toLocalDate, dateEnd)
        for {
          previousHistory <- tryPreviousHistory
          posteriorHistory <- tryPosteriorHistory
        } yield previousHistory ++ currentHistory.filter(
          _.getDateTime.toLocalDate isBefore maxDateTime.toLocalDate
        ) ++ posteriorHistory
      }
    tryNewHistory match
      case Success(newHistory) => writeHistory(fillMissingDailyData(newHistory), asset, false)
      case Failure(ex) => println(s"[Warning] Couldn't update asset $asset. Reason: ${ex.getMessage}")
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
    Option(montoOperado),
    volumenNominal,
    Option(cantidadOperaciones)
  )
}
