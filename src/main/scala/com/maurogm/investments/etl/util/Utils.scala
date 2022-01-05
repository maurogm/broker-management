package com.maurogm.investments.etl.util

import com.maurogm.investments.etl.market.iolAPI.DailyData
import com.maurogm.investments.etl.util.CSVParser
import play.api.libs.json.{JsValue, Json}
import requests.Response

import java.io.{BufferedWriter, File, FileWriter}
import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.util.{Try, Using}

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

  def readFromCSV[T: CSVParser](filename: String): Try[Seq[T]] = {
    readFileAsSeq(filename: String).map(xs => xs.map(CSVParser.fromCsv))
  }

  def writeAsCsv[T](
      data: Seq[T with CSVSerializer],
      filePath: String,
      append: Boolean,
      headers: Option[Seq[String]] = None
  ): Unit = {
    val bw = BufferedWriter(FileWriter(File(filePath), append))
    if (headers.isDefined) bw.write(headers.mkString(",") + "\n")
    data.map(_.toCsv + "\n").foreach(bw.write)
    bw.close()
  }

  def getListOfFiles(dir: File): Seq[String] = dir.listFiles
    .withFilter(_.isFile) // list only files
    .map(_.getName)
    .toList
  def getListOfCSVs(path: String): Seq[String] =
    getListOfFiles(File(path)).filter(_.endsWith(".csv"))

}
