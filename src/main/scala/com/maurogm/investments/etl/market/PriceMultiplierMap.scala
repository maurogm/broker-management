package com.maurogm.investments.etl.market

import com.maurogm.investments.Asset
import com.maurogm.investments.etl.util.CSVParser
import com.maurogm.investments.etl.util.Utils.{readFileAsSeq, readFromCSV}

import scala.util.Try

/** Since the price for some Assets is expressed not by unit but by X units of
  * the asset (for example, bonds' prices are usually given as the price per 100
  * units of the bond), this class keeps tabs on the multiplier needed to
  * convert a given price into a _per unit_ price (in the case of bonds, it
  * would be 0.01).
  */
type PriceMultiplierMap = Map[Asset, BigDecimal]

object PriceMultiplierMap {

  def readPriceMultipliersFromResources: PriceMultiplierMap = {
    readPriceMultipliersFromLocalFile(
      "src/main/resources/market/assetPriceMultipliers.csv"
    )
  }

  def readPriceMultipliersFromLocalFile(
      filename: String
  ): PriceMultiplierMap = {
    given csvParserPriceMultiplier: CSVParser[(Asset, BigDecimal)] with {
      override def fromCsv(str: String): (Asset, BigDecimal) = {
        val tokens = str.split(";")
        val (exchange, ticker, multiplier) = (tokens(0), tokens(1), tokens(2))
        (Asset(exchange, ticker), BigDecimal(multiplier))
      }
    }

    readFromCSV[(Asset, BigDecimal)](
      filename
    ).get.toMap.asInstanceOf[PriceMultiplierMap]
  }
}
