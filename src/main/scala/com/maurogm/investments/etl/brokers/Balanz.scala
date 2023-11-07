package com.maurogm.investments.etl.brokers

import com.maurogm.investments
import com.maurogm.investments.Asset
import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.Utils.{dateParser, dateTimeParser, readFileAsSeq}
import com.maurogm.investments.{Movement, MovementType, Order}

import java.time.format.DateTimeFormatter
import java.time.{LocalDate, LocalDateTime}

class Balanz extends Broker[Balanz] {

  override def broker: String = Balanz.broker

  private def parseSpreadsheetOrder(line: String): ParsedOrderBalanz = {
    val tokens = line.split(";")
    ParsedOrderBalanz(
      tokens(0),
      tokens(1),
      tokens(2),
      tokens(3),
      tokens(4),
      tokens(5),
      tokens(6),
      tokens(7).toDouble,
      tokens(8).toDouble,
      tokens(9).toDouble,
      tokens(10).toDouble,
      tokens(11).toDouble
    )
  }
  private def parseSpreadsheetMovement(line: String): ParsedMovementBalanz = {
    val tokens = line.split(";")
    val parseDate: String => LocalDate = dateParser("yyyy-MM-dd")

    ParsedMovementBalanz(
      tokens(0),
      if (tokens(1) == "") Option.empty else Option(tokens(1)),
      if (tokens(2) == "") Option.empty else Option(tokens(2)),
      parseDate(tokens(3)),
      tokens(4),
      tokens(5),
      parseDate(tokens(6)),
      tokens(7),
      tokens(8).toDouble
    )
  }

  override def readBrokerOrdersFromLocalFile(
                                              path: String
                                            ): Seq[ParsedOrderBalanz] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines
      .tail
      .withFilter(!_.startsWith("Transferencia")) //drop non-order rows
      .map(parseSpreadsheetOrder)
      .filter(_.estado == "Ejecutada")
  }

  override def readBrokerMovementsFromLocalFile(
                                                 path: String
                                               ): Seq[ParsedMovementBalanz] = {
    val lines: Seq[String] = readFileAsSeq(path).get
    lines.tail
      .map(parseSpreadsheetMovement)
      .filterNot(_.toMovement.movementType == MovementType.Operation) //I get this data from the Orders
  }
}

object Balanz {
  val broker: String = "Balanz"
}

case class ParsedMovementBalanz(descripcion: String,
                                ticker: Option[String],
                                tipoInstrumento: Option[String],
                                concertacion: LocalDate,
                                cantidad: String,
                                precio: String,
                                liquidacion: LocalDate,
                                moneda: String,
                                importe: Double
                               ) extends ParsedMovement[Balanz] {
  private def parseDescription(
                                   str: String
                                 ): (MovementType, Option[String]) = str match {
    case s if s.startsWith("Recibo de Cobro") => (MovementType.CashDeposit, None)
    case s if s.startsWith("Comprobante de Pago") => (MovementType.CashWithdrawal, None)
    case s if s.startsWith("Cargo por Descubierto") => (MovementType.Costs, None)
    case s if s.startsWith("Boleto") => (MovementType.Operation, ticker)
    case s if s.startsWith("Renta") => (MovementType.Interests, ticker)
    case s =>
      throw new RuntimeException(s"Couldn't parse the MovementType of $s")
  }

  private def parseCurrency(moneda: String) = {
    if (moneda == "Pesos") "ARS"
    else if (moneda.startsWith("US Dollar")) "USD"
    else if (moneda.startsWith("Dólares")) "USD"
    else
      throw new RuntimeException(
        s"Couldn't parse the currency from tipoCuenta='$moneda'"
      )
  }

  override def toMovement: Movement = {
    val (movementType, maybeTicker) = parseDescription(descripcion)
    investments.Movement(
      Balanz.broker,
      concertacion,
      movementType,
      maybeTicker,
      Money(parseCurrency(moneda), BigDecimal(importe))
    )
  }
}

case class ParsedOrderBalanz(
                              operacion: String,
                              estado: String,
                              idOrden: String,
                              ticker: String,
                              moneda: String,
                              fecha: String,
                              hora: String,
                              cantidad: Double,
                              precio: Double,
                              monto: Double,
                              precioOperado: Double,
                              cantidadOperada: Double
                         ) extends ParsedOrder[Balanz] {
  private def parseOperacion(s: String): String = {
    if (s.startsWith("Compra")) "buy"
    else if (s.startsWith("Venta")) "sell"
    else throw new RuntimeException(s"Couldn't parse the operationType of $s")
  }

  private val currency = if (moneda == "Pesos") "ARS"
    else if (moneda.startsWith("US Dollar")) "USD"
    else
      throw new RuntimeException(
        s"Couldn't parse the currency from tipoCuenta='$moneda'"
      )

  private val parseDateTime: String => LocalDateTime = dateTimeParser(
    "dd/MM/yyyyHH:mm:ss"
  )

  override def toOrder: Order = investments.Order(
    Balanz.broker,
    parseDateTime(fecha + hora),
    Asset("BCBA", ticker),
    parseOperacion(operacion),
    cantidad,
    Money(currency, BigDecimal(precio)),
    Money(currency, BigDecimal(0)),
    Money(currency, BigDecimal(monto))
  )
}
