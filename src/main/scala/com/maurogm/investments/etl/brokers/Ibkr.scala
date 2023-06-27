package com.maurogm.investments.etl.brokers
import com.maurogm.investments.{Movement, Order}


import com.maurogm.investments
import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.Utils.StringParsingExtensions.{parseLatinNumber, remove$, removeQuotations}
import com.maurogm.investments.etl.util.Utils.{dateParser, dateTimeParser, readFileAsSeq}
import com.maurogm.investments.{Movement, MovementType, Order}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class Ibkr extends Broker[Ibkr] {

  override def broker: String = Ibkr.broker

  private def parseSpreadsheetOrder(line: String): ParsedOrderIbkr = {
    val parseDateTime: String => LocalDateTime = dateTimeParser(
      "yyyyMMdd HHmmss"
    )

    val tokens = line.removeQuotations.split(",")
    ParsedOrderIbkr(
      tokens(0),
      parseDateTime(tokens(1)),
      tokens(2),
      tokens(3).toDouble,
      tokens(4).toDouble,
      tokens(5).toDouble,
      tokens(6).toDouble,
      tokens(7),
      tokens(8).toDouble,
      tokens(9),
      tokens(10)
    )
  }
  private def parseSpreadsheetMovement(line: String): ParsedMovementIbkr = {
    val tokens = line.removeQuotations.split(",")
    val parseDate: String => LocalDate = dateParser("yyyyMMdd")

    ParsedMovementIbkr(
      tokens(0),
      tokens(1),
      parseDate(tokens(2).substring(0, 8)),
      tokens(3).toDouble,
      tokens(4)
    )
  }

  override def readBrokerOrdersFromLocalFile(
                                              path: String
                                            ): Seq[ParsedOrderIbkr] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines
      .tail
      .map(parseSpreadsheetOrder)
      .filterNot(_.exchange == "IDEALFX")
  }

  override def readBrokerMovementsFromLocalFile(
                                                 path: String
                                               ): Seq[ParsedMovementIbkr] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines.tail
      .map(parseSpreadsheetMovement)
      .filterNot(_.rawType == "Other Fees")
  }
}

object Ibkr {
  val broker: String = "IBKR"
}

case class ParsedMovementIbkr(currencyPrimary: String,
                                Symbol: String,
                                date: LocalDate,
                                amount: Double,
                              rawType: String
                             ) extends ParsedMovement[Ibkr] {

  private def parseTipoMovimiento(
                                   str: String,
                                   amount: Double
                                 ): (MovementType, Option[String]) = str match {
    case s if s == "Deposits/Withdrawals" && amount < 0 => (MovementType.CashWithdrawal, None)
    case s if s == "Deposits/Withdrawals" && amount > 0 => (MovementType.CashDeposit, None)
    case s =>
      throw new RuntimeException(s"Couldn't parse the MovementType of $s")
  }

  private def validateCurrency: String = if (currencyPrimary == "USD") "USD" else
      throw new RuntimeException(
        s"Couldn't validate currency '$currencyPrimary'. Only USD is supported."
      )

  override def toMovement: Movement = {
    val (movementType, maybeTicker) = parseTipoMovimiento(rawType, amount)
    investments.Movement(
      Ibkr.broker,
      date,
      movementType,
      maybeTicker,
      Money(validateCurrency, BigDecimal(amount))
    )
  }
}

case class ParsedOrderIbkr(
                            symbol: String,
                            dateTime: LocalDateTime,
                            exchange: String,
                            quantity: Double,
                            tradePrice: Double,
                            taxes: Double,
                            iBCommission: Double,
                            iBCommissionCurrency: String,
                            netCash: Double,
                            buyOrSell: String,
                            levelOfDetail: String
                         ) extends ParsedOrder[Ibkr] {
  private val parsedCurrency: String = parseCurrency(iBCommissionCurrency)

  private def parseBuyOrSell(s: String): String = {
    if (s == "BUY") "buy"
    else if (s == "SELL") "sell"
    else throw new RuntimeException(s"Couldn't parse the operationType of $s")
  }

  private def parseCurrency(s: String): String = {
    if (s == "USD") "USD"
    else throw new RuntimeException(s"Couldn't parse the currency of $s")
  }

  private def parseExchange(str: String) =
    if (str == "LSEETF") "LON"
    else throw new RuntimeException(s"Couldn't parse the exchange '$str''")

  override def toOrder: Order = investments.Order(
    Ibkr.broker,
    dateTime,
    Asset(parseExchange(exchange), symbol),
    parseBuyOrSell(buyOrSell),
    quantity,
    Money(parsedCurrency, tradePrice),
    Money(parsedCurrency, taxes + iBCommission),
    Money(parsedCurrency, netCash)
  )
}
