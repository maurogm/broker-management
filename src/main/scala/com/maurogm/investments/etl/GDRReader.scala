package com.maurogm.investments.etl

import com.maurogm.investments.{Asset, GDR}
import com.maurogm.investments.etl.util.Utils.readFromCSV

object GDRReader {
  val adrs: Seq[GDR] = readFromCSV[GDR]("src/main/resources/gdr/adrs.csv").get
  val cedears: Seq[GDR] =
    readFromCSV[GDR]("src/main/resources/gdr/cedears.csv").get
  val gdrs: Seq[GDR] = adrs ++ cedears

  def getByAsset(asset: Asset): GDR = gdrs.filter(_.gdr == asset).head
}
