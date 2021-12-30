package com.maurogm.investments.etl.util

import com.maurogm.investments.Money
import com.maurogm.investments.etl.market.iolAPI.DailyData

import java.time.LocalDateTime

trait CSVSerializer {
  def toCsv: String
}

// 1 - type class definition
trait CSVParser[T] {
  def fromCsv(str: String): T
}
// 2 - type class instances
// Defined in each class' companion object

// 3 - user-facing API
object CSVParser {
  // often similar to what the type class definition offers
  def fromCsv[T](csv: String)(using instance: CSVParser[T]): T =
    instance.fromCsv(csv)
  // often expose a method to retrieve the current given instance for a type (similar to summon)
  def apply[T](using instance: CSVParser[T]): CSVParser[T] = instance
}

// 4 - expressiveness through extension methods
object CSVParserSyntax {
  extension [T](csv: String)
    def fromCsv(using instance: CSVParser[T]): T =
      instance.fromCsv(csv)
}
