package com.maurogm.investments.etl.brokers

import com.maurogm.investments
import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.Utils.{dateParser, dateTimeParser, readFileAsSeq}
import com.maurogm.investments.{Movement, MovementType, Order}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

class Manual extends Broker[Manual] {

  override def broker: String = Manual.broker

  private def parseSpreadsheetOrder(line: String): ParsedOrderManual = {
    val tokens = line.split(",")
    ParsedOrderManual(
      tokens(0),
      tokens(1),
      tokens(2),
      tokens(3),
      tokens(4),
      tokens(5).toDouble,
      tokens(6),
      tokens(7).toDouble,
      tokens(8),
      tokens(9).toDouble,
      tokens(10),
      tokens(11).toDouble
    )
  }
  private def parseSpreadsheetMovement(line: String): ParsedMovementManual = {
    val tokens = line.split(",")

    ParsedMovementManual(
      tokens(0),
      tokens(1),
      tokens(2),
      tokens(3),
      tokens(4),
      tokens(5).toDouble,
      tokens(6)
    )
  }

  override def readBrokerOrdersFromLocalFile(
                                              path: String
                                            ): Seq[ParsedOrderManual] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines
      .tail
      .map(parseSpreadsheetOrder)
  }

  override def readBrokerMovementsFromLocalFile(
                                                 path: String
                                               ): Seq[ParsedMovementManual] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines.tail
      .map(parseSpreadsheetMovement)
  }
}

object Manual {
  val broker: String = "Manual"
}

case class ParsedMovementManual(broker: String,
                                date: String,
                                movementType: String,
                                ticker: String,
                                currency: String,
                                amount: Double,
                                detalles: String
                               ) extends ParsedMovement[Manual] {
  private def parseMovementType(str: String): MovementType = str match {
    case s if s.startsWith("CashDeposit") => MovementType.CashDeposit
    case s =>
      throw new RuntimeException(s"Couldn't parse the MovementType of $s")
  }

  private val parsedTicker: Option[String] = if (ticker == "") None else Option(ticker)

  override def toMovement: Movement = {
    investments.Movement(
      broker,
      LocalDate.parse(date),
      parseMovementType(movementType),
      parsedTicker,
      Money(currency, BigDecimal(amount))
    )
  }
}

case class ParsedOrderManual(
                              broker: String,
                              datetime: String,
                              exchange: String,
                              ticker: String,
                              operationType: String,
                              quantity: Double,
                              price_currency: String,
                              price_amount: Double,
                              costs_currency: String,
                              costs_amount: Double,
                              total_currency: String,
                              total_amount: Double) extends ParsedOrder[Manual] {

  override def toOrder: Order = investments.Order(
    broker,
    LocalDateTime.parse(datetime),
    Asset(exchange, ticker),
    operationType,
    quantity,
    Money(price_currency, BigDecimal(price_amount)),
    Money(costs_currency, BigDecimal(costs_amount)),
    Money(total_currency, BigDecimal(total_amount))
  )
}
