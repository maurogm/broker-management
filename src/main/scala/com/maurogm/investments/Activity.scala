package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.brokers.Broker
import com.maurogm.investments.etl.market.AssetExtension.{
  getHistoricPriceHomogeneous,
  getMostRecentPriceHomogeneous
}
import com.maurogm.investments.etl.market.PriceMultiplierMap
import com.maurogm.investments.etl.util.CurrencyHomogenizerSyntax.homogenizeCurrency
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
  ): Map[Asset, Money] = portfolio.map { case (asset, position) => {
    asset -> (dateOfValuation match {
      case None =>
        asset.getMostRecentPriceHomogeneous * (pmm(asset) * position.total)
      case Some(date) =>
        asset.getHistoricPriceHomogeneous(date) * (pmm(asset) * position.total)
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
