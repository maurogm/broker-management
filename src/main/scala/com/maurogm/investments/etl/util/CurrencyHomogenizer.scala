package com.maurogm.investments.etl.util

import com.maurogm.investments.currency.CurrencyConverter
import com.maurogm.investments.etl.market.iolAPI.DailyData

// 1 - type class definition
trait CurrencyHomogenizer[T](using cc: CurrencyConverter) {
  def homogenizeCurrency(x: T): T
}

// 2 - type class instances
// Defined in each class' companion object

// 3 - user-facing API
object CurrencyHomogenizer {
  // often similar to what the type class definition offers
  def homogenizeCurrency[T](x: T)(using
      instance: CurrencyHomogenizer[T]
  ): T =
    instance.homogenizeCurrency(x)
  // often expose a method to retrieve the current given instance for a type (similar to summon)
  def apply[T](using instance: CurrencyHomogenizer[T]): CurrencyHomogenizer[T] =
    instance
}

// 4 - expressiveness through extension methods
object CurrencyHomogenizerSyntax {
  extension [T](x: T)
    def homogenizeCurrency(using
        instance: CurrencyHomogenizer[T]
    ): T =
      instance.homogenizeCurrency(x)
}
