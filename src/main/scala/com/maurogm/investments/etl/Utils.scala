package com.maurogm.investments.etl

import java.io.File
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.util.{Try, Using}
import requests.Response
import play.api.libs.json.{JsValue, Json}


object Utils {

  object StringParsingExtensions {
    extension (str: String) {
      def dropPoint: String = str.replace(".", "")
      def replaceDecimal: String = str.replace(",", ".")
      def remove$ : String = str.replace("$", "")
      def parseLatinNumber: String = str.dropPoint.replaceDecimal
    }
  }

  object ResponseExtensions {
    extension (response: Response) {
      def toString: String = response.text()
      def toJson: JsValue = Json.parse(response.text())
    }
  }

  def dateParser(formatterStr: String)(date: String): LocalDate =
    LocalDate.parse(date, DateTimeFormatter.ofPattern(formatterStr))
  def dateTimeParser(formatterStr: String)(date: String): LocalDateTime = {
    LocalDateTime.parse(date, DateTimeFormatter.ofPattern(formatterStr))
  }

  def isDollarBond(ticker: String): Boolean = {
    val dollarBondRegexp = "(AL|GD|AE)\\d\\d(D|C)?"
    ticker.matches(dollarBondRegexp)
  }

  def readFileAsSeq(filename: String): Try[Seq[String]] = {
    Using(io.Source.fromFile(filename)) { bufferedSource =>
      val ucLines = for {
        line <- bufferedSource.getLines
      } yield line
      ucLines.toSeq
    }
  }
  def getListOfFiles(dir: File): Seq[String] = dir.listFiles
    .withFilter(_.isFile) // list only files
    .map(_.getName)
    .toList
  def getListOfCSVs(path: String): Seq[String] =
    getListOfFiles(File(path)).filter(_.endsWith(".csv"))
}
