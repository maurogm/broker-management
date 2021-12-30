package com.maurogm.investments.etl.brokers

import com.maurogm.investments.etl.util.Utils.getListOfCSVs
import com.maurogm.investments.{Money, Movement, Order}

import java.time.LocalDate
//import com.maurogm.brokers.etl.*

import java.time.LocalDateTime

trait Broker[A <: Broker[A]] { self: A =>
  def broker: String
  val standardResourcesPath = s"src/main/resources/${broker.toLowerCase}"

  def readBrokerOrdersFromLocalFile(path: String): Seq[ParsedOrder[A]]

  def readBrokerMovementsFromLocalFile(path: String): Seq[ParsedMovement[A]]

  def readOrdersFromResources: Seq[ParsedOrder[A]] = getListOfCSVs(
    s"$standardResourcesPath/orders"
  ).map(file => s"$standardResourcesPath/orders/$file")
    .flatMap(readBrokerOrdersFromLocalFile)

  def readMovementsFromResources: Seq[ParsedMovement[A]] = getListOfCSVs(
    s"$standardResourcesPath/movements"
  ).map(file => s"$standardResourcesPath/movements/$file")
    .flatMap(readBrokerMovementsFromLocalFile)
}

trait ParsedOrder[Broker] {
  def toOrder: Order
}

trait ParsedMovement[Broker] {
  def toMovement: Movement
}
