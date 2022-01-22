package com.maurogm.investments.currency

import com.maurogm.investments.etl.GDRReader
import com.maurogm.investments.etl.market.AssetExtension.getAssetCurrency
import com.maurogm.investments.etl.market.iolAPI.Utils.assetHistoricalRatio
import com.maurogm.investments.{Asset, GDR}

import java.time.LocalDate
import scala.annotation.tailrec

object CurrencyFactory {

  def fromAssetRatio(
      assetNum: Asset,
      assetDen: Asset,
      newCurrencySymbol: String,
      aliases: Option[Set[String]] = None
  ): CurrencyConverter = {
    val ratios = assetHistoricalRatio(assetNum, assetDen)
    val oldCurrencySymbol = assetNum.getAssetCurrency
    new CurrencyConverter(
      newCurrencySymbol,
      ratios.map { case (k, v) => (oldCurrencySymbol, k) -> v },
      aliases
    )
  }

  @tailrec
  def fromGDR(
      gdr: GDR,
      newCurrencySymbol: String,
      toGDRsCurrency: Boolean,
      aliases: Option[Set[String]] = None
  ): CurrencyConverter = {
    if (toGDRsCurrency) {
      val ratios = assetHistoricalRatio(gdr.gdr, gdr.underlying).view
        .mapValues(_ / gdr.ratio)
        .toMap
      val oldCurrencySymbol = gdr.gdr.getAssetCurrency
      new CurrencyConverter(
        newCurrencySymbol,
        ratios.map { case (k, v) => (oldCurrencySymbol, k) -> v },
        aliases
      )
    } else {
      val inverted = GDR(gdr.underlying, gdr.gdr, 1 / gdr.ratio)
      fromGDR(inverted, newCurrencySymbol, true, aliases)
    }
  }
}
