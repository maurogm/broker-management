package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.market.iolAPI.HistoricalData.readHistoryFromCsv

import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object Utils {
  def getAssetCurrency(asset: Asset): String =
    readHistoryFromCsv(asset).head.currency

  private def getClosingPrices(asset: Asset): Map[LocalDate, Money] =
    readHistoryFromCsv(asset).map { x =>
      (x.datetime.toLocalDate, x.lastPrice)
    }.toMap

  def assetHistoricalRatio(
      assetNum: Asset,
      assetDen: Asset
  ): Map[LocalDate, BigDecimal] = {
    val mapNum = getClosingPrices(assetNum)
    val mapDen = getClosingPrices(assetDen)
    val commonDates: Set[LocalDate] = mapNum.keySet.intersect(mapDen.keySet)

    commonDates.map(k => k -> mapNum(k).amount / mapDen(k).amount).toMap
  }

}
