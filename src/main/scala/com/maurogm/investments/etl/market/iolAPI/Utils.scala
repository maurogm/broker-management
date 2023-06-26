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
  ): Either[String, Map[LocalDate, BigDecimal]] = {
    for {
      mapNum <- assetNum.getClosingPrices
      mapDen <- assetDen.getClosingPrices
      commonDates = mapNum.keySet.intersect(mapDen.keySet)
    } yield commonDates.map(k => k -> mapNum(k).amount / mapDen(k).amount).toMap
  }

}
