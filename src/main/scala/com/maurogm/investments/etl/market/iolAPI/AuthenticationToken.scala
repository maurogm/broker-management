package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.etl.Utils.ResponseExtensions.toJson
import com.maurogm.investments.util.ConfigModule
import play.api.libs.json.{JsValue, Json}
import requests.Response

class AuthenticationToken {
  private var tokenResponse: Response = AuthenticationToken.requestNewToken
  private var accessToken: String = getAccessToken(tokenResponse)
  private var refreshToken: String = getRefreshToken(tokenResponse)

  def getAccessToken: String = accessToken

  def refresh(): Unit = {
    tokenResponse = AuthenticationToken.requestRefresh(refreshToken)
    accessToken = getAccessToken(tokenResponse)
    refreshToken = getRefreshToken(tokenResponse)
  }

  private def getAccessToken(tokenResponse: Response): String =
    responseToMap(tokenResponse)("access_token").as[String]

  private def getRefreshToken(tokenResponse: Response): String =
    responseToMap(tokenResponse)("refresh_token").as[String]

  private def responseToMap(response: Response): Map[String, JsValue] =
    response.toJson.as[Map[String, JsValue]]
}

object AuthenticationToken extends ConfigModule {
  val username: String = secrets.getString("iol.username")
  val password: String = secrets.getString("iol.password")

  def requestNewToken: Response = {
    val tokenData = Map(
      "username" -> username,
      "password" -> password,
      "grant_type" -> "password"
    )
    requests.post(url = URLs.TOKEN_URL, data = tokenData)
  }

  def requestRefresh(refreshToken: String): Response = {
    val tokenData = Map(
      "refresh_token" -> refreshToken,
      "grant_type" -> "refresh_token"
    )
    requests.post(url = URLs.TOKEN_URL, data = tokenData)
  }
}
