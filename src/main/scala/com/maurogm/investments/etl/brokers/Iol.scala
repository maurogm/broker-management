package com.maurogm.investments.etl.brokers

import com.maurogm.investments
import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.Utils.StringParsingExtensions.{parseLatinNumber, remove$}
import com.maurogm.investments.etl.util.Utils.{dateParser, dateTimeParser, readFileAsSeq}
import com.maurogm.investments.{Movement, MovementType, Order}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}
import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

class Iol extends Broker[Iol] {

  override def broker: String = Iol.broker

  private def parseSpreadsheetOrder(line: String): ParsedOrderIol = {
    val parseDate: String => LocalDate = dateParser("d/M/yyyy")
    val parseDateTime: String => LocalDateTime = dateTimeParser(
      "d/M/yyyy HH:mm:ss"
    )

    val tokens = line.split(";")
    ParsedOrderIol(
      parseDateTime(tokens(0)),
      parseDate(tokens(1)),
      tokens(2),
      tokens(3),
      tokens(4),
      tokens(5),
      tokens(6),
      tokens(7),
      tokens(8),
      tokens(9).parseLatinNumber.toDouble,
      tokens(10),
      tokens(11).parseLatinNumber.remove$,
      tokens(12).parseLatinNumber.remove$,
      tokens(13).parseLatinNumber.remove$,
      tokens(14).parseLatinNumber.remove$,
      tokens(15).parseLatinNumber.remove$
    )
  }
  private def parseSpreadsheetMovement(line: String): ParsedMovementIol = {
    val tokens = line.split(";")
    val parseDate: String => LocalDate = dateParser("dd/MM/yy")

    ParsedMovementIol(
      tokens(0).toLong,
      tokens(1).toLong match {
        case 0 => None
        case n => Some(n)
      },
      tokens(2),
      parseDate(tokens(3)),
      parseDate(tokens(4)),
      tokens(5),
      tokens(6).parseLatinNumber.remove$.toDouble,
      tokens(7).parseLatinNumber.remove$,
      tokens(8).parseLatinNumber.remove$,
      tokens(9).parseLatinNumber.remove$,
      tokens(10).parseLatinNumber.remove$,
      tokens(11).parseLatinNumber.remove$,
      tokens(12),
      tokens(13)
    )
  }

  override def readBrokerOrdersFromLocalFile(
      path: String
  ): Seq[ParsedOrderIol] = {
    val lines: Seq[String] = readFileAsSeq(path).get.drop(1)
    // val colNames: Array[String] = lines.head.split(";")
    lines.tail.map(parseSpreadsheetOrder)
  }

  override def readBrokerMovementsFromLocalFile(
      path: String
  ): Seq[ParsedMovementIol] = {
    val lines: Seq[String] = readFileAsSeq(path).get.drop(4)
    lines.tail
      .map(parseSpreadsheetMovement)
      .filterNot(_.tipoMovimiento.matches("(Compra|Venta).+"))
  }
}

object Iol {
  val broker: String = "Iol"
}

case class ParsedMovementIol(
    idMovimiento: Long,
    nroBoleto: Option[Long],
    tipoMovimiento: String,
    fechaTrx: LocalDate,
    fechaLiq: LocalDate,
    status: String,
    cantidad: Double,
    precio: String,
    comision: String,
    iva: String,
    otrosImpuestos: String,
    monto: String,
    observaciones: String,
    tipoCuenta: String
) extends ParsedMovement[Iol] {

  private def parseTipoMovimiento(
      str: String
  ): (MovementType, Option[String]) = str match {
    case s if s == "Crédito"           => (MovementType.Credit, None)
    case s if s == "Débito - Producto" => (MovementType.Costs, None)
    case s if s.startsWith("Transferencia de Titulos IN") => (MovementType.AssetTransferIn, None)
    case s if s.startsWith("Transferencia de Titulos OUT") => (MovementType.AssetTransferOut, None)
    case s if s.startsWith("Depósito") => (MovementType.CashDeposit, None)
    case s if s.startsWith("Extracción") =>
      (MovementType.CashWithdrawal, None)
    case s if s.startsWith("Pago de") =>
      val regex = "Pago de (\\w+)\\((\\w+).*".r
      val regex(kind, ticker) = str
      if (kind == "Dividendos")
        (MovementType.Dividends, Some(ticker))
      else (MovementType.Amortization, Some(ticker))
    case s =>
      throw new RuntimeException(s"Couldn't parse the MovementType of $s")
  }

  private def parseCurrency(tipoCuenta: String) = {
    if (tipoCuenta.endsWith("Pesos")) "ARS"
    else if (tipoCuenta.endsWith("Dolares")) "USD"
    else
      throw new RuntimeException(
        s"Couldn't parse the currency from tipoCuenta='$tipoCuenta'"
      )
  }

  override def toMovement: Movement = {
    val (movementType, maybeTicker) = parseTipoMovimiento(tipoMovimiento)
    investments.Movement(
      Iol.broker,
      fechaTrx,
      movementType,
      maybeTicker,
      Money(parseCurrency(tipoCuenta), BigDecimal(monto))
    )
  }
}

case class ParsedOrderIol(
    fechaTrx: LocalDateTime,
    fechaLiq: LocalDate,
    boleto: String,
    exchange: String,
    tipo: String,
    cuenta: String,
    desc: String,
    especie: String,
    ticker: String,
    cantidad: Double,
    moneda: String,
    precioPonderado: String,
    monto: String,
    comision: String,
    iva: String,
    total: String
) extends ParsedOrder[Iol] {
  private def parseTipo(s: String): String = {
    if (s == "Compra") "buy"
    else if (s == "Venta") "sell"
    else throw new RuntimeException(s"Couldn't parse the operationType of $s")
  }

  private def parseCurrency(s: String): String = {
    if (s == "US$" || s == "USD") "USD"
    else if (s == "AR$" || s == "ARS") "ARS"
    else throw new RuntimeException(s"Couldn't parse the currency of $s")
  }

  private def strToMoney(currency: String, value: String): Money =
    Money(parseCurrency(currency), BigDecimal(value))

  /** Since some USD operations have ARS costs, which are signaled by the `$`
    * symbol, this method is needed to correctly parse such cases.
    */
  private def parseCosts(currency: String, value: String): Money = if (
    value.startsWith("$")
  ) strToMoney("ARS", value.remove$)
  else strToMoney(currency, value)

  override def toOrder: Order = investments.Order(
    Iol.broker,
    fechaTrx,
    Asset(exchange, ticker),
    parseTipo(tipo),
    cantidad,
    strToMoney(moneda, precioPonderado),
    parseCosts(moneda, comision) + parseCosts(moneda, iva),
    strToMoney(moneda, total)
  )
}
