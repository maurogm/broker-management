package com.maurogm.investments.etl.market.iolAPI

import requests.Response

import java.time.LocalDate
import scala.util.Try

object URLs {
  val BASE_URL = "https://api.invertironline.com"
  val TOKEN_URL = s"$BASE_URL/token"

  def historicalDataUrl(
      exchange: String,
      ticker: String,
      dateStart: LocalDate | String,
      dateEnd: LocalDate | String,
      adjust: Boolean
  ): String = {
    val adjustStr: String = if adjust then "ajustada" else "sinAjustar"
    s"$BASE_URL/api/v2/$exchange/Titulos/$ticker/Cotizacion/seriehistorica/$dateStart/$dateEnd/$adjustStr"
  }

  def makeRequest(
      url: String
  )(using authToken: AuthenticationToken): Try[Response] = {
    lazy val response = requests.get(
      url = url,
      headers = Map("Authorization" -> s"Bearer ${authToken.getAccessToken}")
    )
    Try(response)
  }
}
