package com.maurogm.investments.etl.market

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.Utils.readFromCSV

import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object AssetExtension {

  extension (asset: Asset) {
    def localFilepath: String =
      s"src/main/resources/market/${asset.exchange.toUpperCase}/${asset.ticker.toUpperCase}.csv"

    def getAssetCurrency: Either[String, String] = asset.readHistoryFromCsv.map(x => x.head.currency)

    def readHistoryFromCsv: Either[String, Seq[DailyData]] = {
      readFromCSV[DailyData](asset.localFilepath) match {
        case Success(history) => Right(history)
        case Failure(_) => Left(s"Could not find history for $asset")
      }
    }

    def getClosingPrices: Either[String, Map[LocalDate, Money]] = for {
      history <- asset.readHistoryFromCsv
    } yield history.map{dailyData => (dailyData.datetime.toLocalDate, dailyData.lastPrice)}.toMap

    def getClosingPricesHomogeneous(using
        cc: CurrencyConverter
    ): Either[String, Map[LocalDate, Money]] = for {
      history <- asset.readHistoryFromCsv
    } yield history.map { x => (x.datetime.toLocalDate, x.lastPrice.convert(x.datetime.toLocalDate)) }.toMap


    def getHistoricPrice(date: LocalDate): Either[String, Money] = for {
      closingPrices <- asset.getClosingPrices
    } yield closingPrices(date)

    def getHistoricPriceHomogeneous(date: LocalDate)(using cc: CurrencyConverter): Either[String, Money] = for {
      closingPrices <- asset.getClosingPrices
      closingPrice <- closingPrices.get(date).toRight(s"Asset $asset no tiene datos para la fecha $date")
    } yield closingPrice.convert(date)

    def getMostRecentPrice: Either[String, Money] = for {
      closingPrices <- asset.getClosingPrices
      lastDate = closingPrices.keys.max
    } yield closingPrices(lastDate)

    def getMostRecentPriceHomogeneous(using cc: CurrencyConverter): Either[String, Money] = for {
      closingPrices <- asset.getClosingPrices
      lastDate = closingPrices.keys.max
  } yield closingPrices(lastDate).convert(lastDate)

    def getMostRecentDateWithPrice: Either[String, LocalDate] = for {
      closingPrices <- getClosingPrices
    } yield closingPrices.keys.max
  }
}
