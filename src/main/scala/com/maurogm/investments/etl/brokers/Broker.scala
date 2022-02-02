package com.maurogm.investments.etl.brokers

import com.maurogm.investments.etl.util.Utils.getListOfCSVs
import com.maurogm.investments.{Movement, Order}

import java.time.LocalDate
//import com.maurogm.brokers.etl.*

import java.time.LocalDateTime

trait Broker[A <: Broker[A]] { self: A =>
  def broker: String
  val standardResourcesPath = s"src/main/resources/${broker.toLowerCase}"

  def readBrokerOrdersFromLocalFile(path: String): Seq[ParsedOrder[A]]

  def readBrokerMovementsFromLocalFile(path: String): Seq[ParsedMovement[A]]

  def readOrdersFromResources: Seq[Order] = getListOfCSVs(
    s"$standardResourcesPath/orders"
  ).map(file => s"$standardResourcesPath/orders/$file")
    .flatMap(readBrokerOrdersFromLocalFile)
    .map(_.toOrder)

  def readMovementsFromResources: Seq[Movement] = getListOfCSVs(
    s"$standardResourcesPath/movements"
  ).map(file => s"$standardResourcesPath/movements/$file")
    .flatMap(readBrokerMovementsFromLocalFile)
    .map(_.toMovement)
}

trait ParsedOrder[Broker] {
  def toOrder: Order
}

trait ParsedMovement[Broker] {
  def toMovement: Movement
}
