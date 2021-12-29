package com.maurogm.investments.etl.market.iolAPI

import java.time.LocalDate

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
}
