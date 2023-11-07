package com.maurogm.investments.etl.brokers

import com.maurogm.investments
import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.Utils.StringParsingExtensions.{parseLatinNumber, remove$}
import com.maurogm.investments.etl.util.Utils.{dateParser, dateTimeParser, isDollarBond, readFileAsSeq}
import com.maurogm.investments.{Movement, MovementType, Order}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import java.util.Currency
import scala.util.{Failure, Success, Try}

class Cocos extends Broker[Cocos] {

  override def broker: String = Cocos.broker

  val parseDateTime: String => LocalDateTime = dateTimeParser(
    "dd/MM/yy HH:mm:ss"
  )

  private def parseSpreadsheetOrder(
      ticker: String
  )(line: String): ParsedOrderCocos = {
    val tokens = line.split(";")
    ParsedOrderCocos(
      ticker,
      tokens(1),
      tokens(2).toDouble,
      tokens(3).toDouble,
      tokens(4),
      tokens(5),
      parseDateTime(s"${tokens(6)} ${tokens(7)}"),
      tokens(8),
      tokens(9),
      tokens(10)
    )
  }

  def parseSpreadsheetMovement(line: String): ParsedMovementCocos = {
    val lineTokens = line.split(";", -1)
    val moneda = if (lineTokens.length == 11) "USD" else "ARS"
    val tokens =
      if (lineTokens(0) == "DOLAR MEP" | lineTokens(0) == "USD CABLE") lineTokens.tail else lineTokens

    def parseOptions(str: String) = if (str == "") None else Some(str)

    val parseDate: String => LocalDate = dateParser("dd/MM/yy")

    ParsedMovementCocos(
      moneda,
      parseDate(tokens(0)),
      parseDate(tokens(1)),
      tokens(2),
      tokens(3).parseLatinNumber.toDouble,
      parseOptions(tokens(4)),
      parseOptions(tokens(5)).map(_.parseLatinNumber.toDouble),
      parseOptions(tokens(6)),
      tokens(7).parseLatinNumber,
      tokens(8).parseLatinNumber
    )

  }

  override def readBrokerOrdersFromLocalFile(
      path: String
  ): Seq[ParsedOrder[Cocos]] = {
    val lines = readFileAsSeq(path).get.filter(line =>
      !line.startsWith("Total") & line != ";;;;;;;;;;" & !line.startsWith("true")
    )

    val body = lines.drop(3)
    val tickers =
      body.withFilter(_.endsWith(";;;;;;;;;;")).map(_.replaceAll(";", ""))
    body
      .drop(1) // drop first ticker from body
      .withFilter(
        !_.startsWith(";Cpbt")
        //_ != ";Cpbt.;Número;Cantidad;Precio;Importe;Fecha;Hora;Plazo;Vto.;Estado"
      )
      .map(line => if line.endsWith(";;;;;;;;;;") then "TICKER-LINE" else line)
      .mkString("\n")
      .split("TICKER-LINE\n")
      .zip(tickers)
      .flatMap { case (block, ticker) =>
        block.split("\n").map(parseSpreadsheetOrder(ticker))
      }
      .filter(_.estado == "Cumplida")

  }

  override def readBrokerMovementsFromLocalFile(
      path: String
  ): Seq[ParsedMovementCocos] = {
    readFileAsSeq(path).get
      .drop(8)
      .filterNot(_.endsWith(";;;;")) // This removes the rows at the bottom
      .filterNot(_.endsWith("SALDO ANTERIOR")) // This removes summary info from previous movements
      .map(_.replaceFirst(";", "")) // removes empty first column
      .map(parseSpreadsheetMovement)
      .filterNot(_.operacion.matches("(CPRA|VTAS|CPU\\$)"))

  }
}

object Cocos {
  val broker: String = "Cocos"
}

case class ParsedMovementCocos(
    moneda: String,
    fechaLiq: LocalDate,
    fechaTrx: LocalDate,
    operacion: String,
    numero: Double,
    especie: Option[String],
    cantidad: Option[Double],
    precio: Option[String],
    importe: String,
    saldo: String
) extends ParsedMovement[Cocos] {

  private def parseTipoMovimiento(operacion: String): MovementType =
    operacion match {
      case s if s == "CU$S" || s == "COBR" => MovementType.CashDeposit
      case s if s == "CPRC"                => MovementType.CashConversion
      case s if s == "DIV"                 => MovementType.Dividends
      case s if s == "NOCR"                => MovementType.Dividends // I think... I'm not sure what this is
      case s if s == "CU$V"                => MovementType.Dividends // I think... I'm not sure what this is
      case s if s == "RTA"                 => MovementType.Amortization
      case s if s == "PAGO"                => MovementType.CashWithdrawal
      case s if s == "NCCO" || s == "DRIG" => MovementType.Costs
      case s if s == "NDGI"                => MovementType.Costs
      case s if s == "DU$S" || s == "DECU" => MovementType.Costs
      case s if s == "CAEX"                => MovementType.Unknown
      case s =>
        throw new RuntimeException(s"Couldn't parse the MovementType of $s")
    }

  private def parseEspecie(especie: String): String = { // TODO: Parsear en serio esto
    especie
  }

  override def toMovement: Movement = {
    investments.Movement(
      Cocos.broker,
      fechaTrx,
      parseTipoMovimiento(operacion),
      especie.map(parseEspecie),
      Money(moneda, BigDecimal(importe))
    )
  }
}

case class ParsedOrderCocos(
    ticker: String,
    cpbt: String,
    numero: Double,
    cantidad: Double,
    precio: String,
    importe: String,
    datetime: LocalDateTime,
    plazo: String,
    vencimiento: String,
    estado: String
) extends ParsedOrder[Cocos] {

  private def parseCpbt(s: String) = {
    if (s == "CPRA" || s == "CPU$" || s == "CPRC") "buy"
    else if (s == "VTAS") "sell"
    else throw new RuntimeException(s"Couldn't parse the operationType of $s")
  }

  private def parseMoney(ticker: String) =
    if (isDollarBond(ticker) && ticker.length == 5) "USD" else "ARS"

  override def toOrder: Order =
    investments.Order(
      Cocos.broker,
      datetime,
      Asset("BCBA", ticker),
      parseCpbt(cpbt),
      cantidad,
      Money(parseMoney(ticker), BigDecimal(precio)),
      Money("ARS", 0),
      Money(parseMoney(ticker), BigDecimal(importe))
    )
}
