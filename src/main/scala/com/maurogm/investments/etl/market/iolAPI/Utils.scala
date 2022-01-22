package com.maurogm.investments.etl.market.iolAPI

import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.market.AssetExtension.getClosingPrices

import java.time.LocalDate
import scala.util.{Failure, Success, Try}

object Utils {
  def assetHistoricalRatio(
      assetNum: Asset,
      assetDen: Asset
  ): Map[LocalDate, BigDecimal] = {
    val mapNum = assetNum.getClosingPrices
    val mapDen = assetDen.getClosingPrices
    val commonDates: Set[LocalDate] = mapNum.keySet.intersect(mapDen.keySet)

    commonDates.map(k => k -> mapNum(k).amount / mapDen(k).amount).toMap
  }

}
