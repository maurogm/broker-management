package com.maurogm.investments.etl.market

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.Utils.readFromCSV

import java.time.LocalDate
import scala.util.{Failure, Success}

object AssetExtension {

  extension (asset: Asset) {
    def localFilepath: String =
      s"src/main/resources/market/${asset.exchange.toUpperCase}/${asset.ticker.toUpperCase}.csv"

    def getAssetCurrency: String = asset.readHistoryFromCsv.head.currency

    def readHistoryFromCsv: Seq[DailyData] = {
      readFromCSV[DailyData](asset.localFilepath) match {
        case Success(history) => history
        case Failure(_) =>
          throw new NoSuchElementException(s"Could not find history for $asset")
      }
    }

    def getClosingPrices: Map[LocalDate, Money] =
      asset.readHistoryFromCsv.map { x =>
        (x.datetime.toLocalDate, x.lastPrice)
      }.toMap

    def getClosingPricesHomogeneous(using
        cc: CurrencyConverter
    ): Map[LocalDate, Money] =
      asset.readHistoryFromCsv.map { x =>
        (x.datetime.toLocalDate, x.lastPrice.convert(x.datetime.toLocalDate))
      }.toMap

    def getMostRecentPrice: Money = {
      val closingPrices = asset.getClosingPrices
      val lastDate = closingPrices.keys.max
      closingPrices(lastDate)
    }

    def getMostRecentPriceHomogeneous(using cc: CurrencyConverter): Money =
      val closingPrices = asset.getClosingPrices
      val lastDate = closingPrices.keys.max
      closingPrices(lastDate).convert(lastDate)
  }

}
