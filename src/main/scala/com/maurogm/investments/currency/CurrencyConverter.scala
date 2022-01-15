package com.maurogm.investments.currency

import java.time.LocalDate
import scala.util.Try

/** Used to store the exchange rates between a certain Currency and other
  * currencies at different points in time.
  *
  * @param symbol
  *   The symbol of the Currency of reference.
  * @param table
  *   A Map that given a currency and a date returns the exchange rate to
  *   `symbol`.
  * @param aliases
  *   Alternative denominations for `symbol`. Used to know that the exchange
  *   rate between them and `symbol` is always 1, without having to add entries
  *   to the `table`.
  */
class CurrencyConverter(
    val symbol: String,
    val table: Map[(String, LocalDate), BigDecimal],
    val aliases: Option[Set[String]] = None
) {
  def convert(money: Money, date: LocalDate): Money = {
    if (aliases.exists(_.contains(money.currency))) {
      Money(symbol, money.amount)
    } else {
      val maybeRate = Try(table(money.currency, date))
      if maybeRate.isSuccess then money.convertTo(symbol, maybeRate.get)
      else {
        throw new NoSuchElementException(
          s"An exchange rate was needed for currency ${money.currency} and date $date, but none was found"
        )
      }
    }
  }

  /** Gets a new CurrencyConverter with just the dates in a range.
    *
    * Intended as a first step towards concatenating different
    * CurrencyConverters.
    */
  def trimDates(
      minDate: LocalDate = LocalDate.MIN,
      maxDate: LocalDate = LocalDate.MAX
  ): CurrencyConverter = {
    val trimmedTable = table.filter { case ((_, date), _) =>
      (!minDate.isAfter(date)) && (date isBefore maxDate)
    }
    new CurrencyConverter(symbol, trimmedTable, aliases)
  }

  /** Adds the entire table of a new CurrencyConverter to this
    * CurrencyConverter's table.
    *
    * In case of repeated keys, the new one's values are kept.
    */
  def addOrUpdate(
      that: CurrencyConverter,
      newSymbol: Option[String] = None
  ): CurrencyConverter = {
    new CurrencyConverter(
      newSymbol.getOrElse(symbol),
      this.table ++ that.table,
      aliases
    )
  }

  // TODO? Agregar la posibilidad de a partir de un CurrencyConverter (de CURR1) y las relaciones históricas entre CURR2 y CURR1, crear un CurrencyConverter de CURR2. Esto serviría para pasar monedas a valor presente.

}
