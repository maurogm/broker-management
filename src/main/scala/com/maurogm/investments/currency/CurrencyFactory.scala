package com.maurogm.investments.currency

import com.maurogm.investments.etl.GDRReader
import com.maurogm.investments.etl.market.iolAPI.HistoricalData.readHistoryFromCsv
import com.maurogm.investments.etl.market.iolAPI.Utils.{
  assetHistoricalRatio,
  getAssetCurrency
}
import com.maurogm.investments.util.AssetsOfInterest
import com.maurogm.investments.{Asset, GDR}

import java.time.LocalDate
import scala.annotation.tailrec

object CurrencyFactory {

  def fromAssetRatio(
      assetNum: Asset,
      assetDen: Asset,
      newCurrencySymbol: String
  ): CurrencyConverter = {
    val ratios = assetHistoricalRatio(assetNum, assetDen)
    val oldCurrencySymbol = getAssetCurrency(assetNum)
    new CurrencyConverter(
      newCurrencySymbol,
      ratios.map { case (k, v) => (oldCurrencySymbol, k) -> v }
    )
  }

  @tailrec
  def fromGDR(
      gdr: GDR,
      newCurrencySymbol: String,
      toGDRsCurrency: Boolean
  ): CurrencyConverter = {
    if (toGDRsCurrency) {
      val ratios = assetHistoricalRatio(gdr.gdr, gdr.underlying).view
        .mapValues(_ / gdr.ratio)
        .toMap
      val oldCurrencySymbol = getAssetCurrency(gdr.gdr)
      new CurrencyConverter(
        newCurrencySymbol,
        ratios.map { case (k, v) => (oldCurrencySymbol, k) -> v }
      )
    } else {
      val inverted = GDR(gdr.underlying, gdr.gdr, 1 / gdr.ratio)
      fromGDR(inverted, newCurrencySymbol, true)
    }
  }
}
