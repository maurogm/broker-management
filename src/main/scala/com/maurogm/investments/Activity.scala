package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.brokers.Broker
import com.maurogm.investments.etl.market.AssetExtension.{
  getHistoricPriceHomogeneous,
  getMostRecentPriceHomogeneous
}
import com.maurogm.investments.etl.market.PriceMultiplierMap
import com.maurogm.investments.etl.util.CurrencyHomogenizerSyntax.homogenizeCurrency
import com.maurogm.investments.etl.util.Utils.writeAsCsv
import com.maurogm.investments.util.EquivalentAssets

import java.time.LocalDate
import scala.annotation.targetName

type Portfolio = Map[Asset, Position]
extension (portfolio: Portfolio) {

  /** Returns the valuation of the porfolio using the prices each asset had at
    * closing time during the `dateOfValuation`.
    *
    * If no `dateOfValuation` is provided, then the most recent prices available
    * are used.
    */
  def valuation(dateOfValuation: Option[LocalDate] = None)(using
      cc: CurrencyConverter,
      pmm: PriceMultiplierMap
  ): Map[Asset, Money] = portfolio.map {
    case (asset, position) => {
      asset -> (dateOfValuation match {
        case None =>
          asset.getMostRecentPriceHomogeneous * (pmm.getOrElse(asset, BigDecimal(1)) * position.total)
        case Some(date) =>
          asset
            .getHistoricPriceHomogeneous(date) * (pmm.getOrElse(asset, BigDecimal(1)) * position.total)
      })
    }
  }
}

trait ActivityInterface {

  def orders: Seq[Order]
  def movements: Seq[Movement]

  def currentPortfolio: Portfolio

  /** @return
    *   A view of the Activity previous to `date`.
    */
  def pastActivity(date: LocalDate): Activity

  /*
  TODO: Dado un porfolio a una fecha, asignar a cada orden de su historia sus PnLs realizadas y no realizadas.
   */

}

class Activity(orderSeq: Seq[Order], movementSeq: Seq[Movement])(using
    cc: CurrencyConverter
) extends ActivityInterface
    with EquivalentAssets {

  def this(broker: Broker[_])(using cc: CurrencyConverter) = {
    this(broker.readOrdersFromResources, broker.readMovementsFromResources)
  }

  def this(brokers: Seq[Broker[_]])(using cc: CurrencyConverter) = {
    this(
      brokers.foldRight[Seq[Order]](Seq()) { (broker, os) =>
        broker.readOrdersFromResources ++ os
      },
      brokers.foldRight[Seq[Movement]](Seq()) { (broker, ms) =>
        broker.readMovementsFromResources ++ ms
      }
    )
  }

  def orders: Seq[Order] = orderSeq
    .map(homogenizeCurrency)
    .map(o => o.copy(asset = o.asset.resolveAliases(equivalentAssetsDict)))
  def movements: Seq[Movement] = movementSeq.map(homogenizeCurrency)

  infix def +(that: Activity): Activity =
    Activity(this.orders ++ that.orders, this.movements ++ that.movements)

  def pastActivity(date: LocalDate): Activity = Activity(
    orders.filter(_.datetime isBefore date.atStartOfDay().plusDays(1)),
    movements.filter(_.date isBefore date)
  )

  def currentPortfolio: Portfolio = orders
    .groupBy(_.asset)
    .view
    .mapValues(Activity.fromOrdersToPosition)
    .toMap
    .filterNot(
      _._1 == Asset("BCBA", "TX22")
    ) // TODO: Find a permanent fix for the fact that TX22 data is incomplete

  def persistAsCSV(
      filePathsRoot: String = "src/main/resources/outputs/",
      movementsFileName: String = "unified_movements.csv",
      ordersFileName: String = "unified_orders.csv"
  ): Unit = {
    writeAsCsv(
      data = movements,
      filePath = filePathsRoot + movementsFileName,
      append = false,
      headers = Some(
        Seq("broker", "date", "movementType", "ticker", "currency", "amount")
      )
    )
    writeAsCsv(
      data = orders,
      filePath = filePathsRoot + ordersFileName,
      append = false,
      headers = Some(
        Seq(
          "broker",
          "datetime",
          "exchange",
          "ticker",
          "operationType",
          "quantity",
          "price_currency",
          "price_amount",
          "costs_currency",
          "costs_amount",
          "total_currency",
          "total_amount"
        )
      )
    )
  }
}

object Activity {
  def fromOrdersToPosition(xs: Seq[Order]): Position = {
    xs.sorted
      .foldLeft[Position](Position(Nil))((pos, ord) =>
        if (ord.operationType == "buy") {
          pos.buy(ord.quantity.toInt, ord.price, ord.datetime)
        } else if (ord.operationType == "sell") {
          pos.sell(ord.quantity.toInt, ord.price, ord.datetime)._1
        } else
          throw new IllegalArgumentException(
            "Trying to build a Position from an order that is neither buy nor sell"
          )
      )
  }
}
