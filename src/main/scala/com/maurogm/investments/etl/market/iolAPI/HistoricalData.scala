package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Money
import com.maurogm.investments.etl.Utils.ResponseExtensions.toJson
import com.maurogm.investments.etl.market.iolAPI.{
  AuthenticationToken,
  DailyData,
  URLs
}
import play.api.libs.json.{JsValue, Json}
import requests.Response

import java.time.{LocalDate, LocalDateTime}
import scala.util.Try

object IolAPI {
  def main(args: Array[String]): Unit = {

    given authToken: AuthenticationToken = new AuthenticationToken
    val bearerToken = authToken.getAccessToken
    println(bearerToken)

    val history =
      getHistoricalData("bCBA", "AL30", "2021-12-20", "2021-12-26", false)

    println(history.mkString("\n"))
  }

  /** @param exchange
    *   The exchange in which `ticker` is traded. One of `bCBA`, `nYSE`,
    *   `nASDAQ`, `aMEX`, `bCS`, `rOFX`.
    */
  def getHistoricalData(
      exchange: String,
      ticker: String,
      dateStart: LocalDate | String,
      dateEnd: LocalDate | String,
      adjust: Boolean
  )(using authToken: AuthenticationToken): Seq[DailyData] = {
    val url =
      URLs.historicalDataUrl(exchange, ticker, dateStart, dateEnd, adjust)
    makeRequest(url).toJson
      .as[Seq[Map[String, JsValue]]]
      .map(parseHistoricalData(_).toDailyData)
  }

  def makeRequest(
      url: String
  )(using authToken: AuthenticationToken): Response = {
    requests.get(
      url = url,
      headers = Map("Authorization" -> s"Bearer ${authToken.getAccessToken}")
    )
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
