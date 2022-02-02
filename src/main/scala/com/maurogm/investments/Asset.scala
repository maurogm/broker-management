package com.maurogm.investments

import com.maurogm.investments.etl.util.CSVParser

import scala.util.Try

case class Asset(exchange: String, ticker: String) {
  def resolveAliases(aliasTable: Map[Asset, Asset]): Asset = {
    Try(aliasTable(this)).getOrElse(this)
  }
}

/** @param ratio
  *   How many stocks of the underlying asset are contained in one unit of the
  *   gdr
  */
case class GDR(gdr: Asset, underlying: Asset, ratio: BigDecimal)

object GDR {
  given csvParserGDR: CSVParser[GDR] with {
    override def fromCsv(str: String): GDR = {
      val tokens = str.split(";")
      val gdrTicker = tokens(0)
      val underlyingTicker = tokens(1)
      val gdrExchange = tokens(3)
      val underlyingExchange = tokens(4)
      val ratioStr = tokens(2)
      val ratioArray = ratioStr.split(":")
      val ratio = BigDecimal(ratioArray(1).toDouble) / BigDecimal(ratioArray(0).toDouble)
      GDR(Asset(gdrExchange, gdrTicker), Asset(underlyingExchange, underlyingTicker), ratio)
    }
  }
}
