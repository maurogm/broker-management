package com.maurogm.investments.currency

import java.time.LocalDate
import scala.util.Try

class CurrencyConverter(
    val symbol: String,
    val table: Map[(String, LocalDate), BigDecimal]
) {
  def convert(money: Money, date: LocalDate): Money = {
    val rate = Try(table(money.currency, date))
    if rate.isSuccess then money.convertTo(symbol, rate.get)
    else {
      throw new NoSuchElementException(
        s"An exchange rate was needed for currency ${money.currency} and date $date, but none was found"
      )
    }
  }
  //TODO: Agregar la posibilidad de que ciertas monedas tengan ratio 1
  //TODO? Agregar la posibilidad de a partir de un CurrencyConverter (de CURR1) y las relaciones históricas entre CURR2 y CURR1, crear un CurrencyConverter de CURR2. Esto serviría para pasar monedas a valor presente.

}
