package com.maurogm.investments

import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.brokers.Broker
import com.maurogm.investments.etl.market.AssetExtension.{
  getHistoricPrice,
  getHistoricPriceHomogeneous,
  getMostRecentPrice,
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
  def valuation(dateOfValuation: Option[LocalDate] = None, convertCurrency: Boolean = true)(using
      cc: CurrencyConverter,
      pmm: PriceMultiplierMap
  ): Map[Asset, Either[String, Money]] = portfolio.map {
    case (asset, position) => {
      val maybePrice = dateOfValuation match {
        case None if  convertCurrency => asset.getMostRecentPriceHomogeneous
        case None if !convertCurrency => asset.getMostRecentPrice
        case Some(date) if  convertCurrency => asset.getHistoricPriceHomogeneous(date)
        case Some(date) if !convertCurrency => asset.getHistoricPrice(date)
      }
      val maybeTotalValuation = maybePrice.map(_ * (pmm.getOrElse(asset, BigDecimal(1)) * position.total))
      asset -> maybeTotalValuation
    }
  }

  def totalPortfolioValue(dateOfValuation: Option[LocalDate] = None)(using
                                                                     cc: CurrencyConverter,
                                                                     pmm: PriceMultiplierMap
  ): Money = valuation(dateOfValuation)
    .view
    .mapValues(x => x.getOrElse(Money.zero("CCL")))
    .values
    .reduce(_ + _)
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

  lazy val brokers: Set[String] = movements.map(_.broker).toSet ++ orders.map(_.broker).toSet

  def netCashMovements: Money = movements.sorted.foldLeft(Money.zero("CCL"))(_ + _.amount)

  def buysTotal: Money = orders.filter(_.operationType == "buy").foldLeft(Money.zero("CCL"))(_ + _.total)

  def sellsTotal: Money = orders.filter(_.operationType == "sell").foldLeft(Money.zero("CCL"))(_ + _.total)

  def cashApproximation: Money = netCashMovements - buysTotal + sellsTotal

  def getActivityByBroker: Map[String, Activity] = {
    val keyValueSet: Set[(String, Activity)] = for {
      broker <- brokers
      brokerMovements = movements.filter(_.broker == broker)
      brokerOrders = orders.filter(_.broker == broker)
      brokerActivity = new Activity(brokerOrders, brokerMovements)
    } yield broker -> brokerActivity
    keyValueSet.toMap
  }


  def currentPortfolio: Portfolio = orders
    .groupBy(_.asset)
    .view
    .mapValues(Activity.fromOrdersToPosition)
    .toMap

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

  def fromOrdersToPosition(xs: Seq[Order]): Position = fromOrdersToPositionAndSaleResults(xs)._1

  def fromOrdersToSaleResults(xs: Seq[Order]): Iterable[SaleResult] = fromOrdersToPositionAndSaleResults(xs)._2

  def fromOrdersToPositionAndSaleResults(xs: Seq[Order]): (Position, Iterable[SaleResult]) = {
    xs.sorted
      .foldLeft[(Position, Iterable[SaleResult])]((Position(Nil), Iterable.empty)) {case ((pos, saleResults), ord) =>
        if (ord.operationType == "buy") {
          val newPos = pos.buy(ord.quantity.toInt, ord.price, ord.datetime)
          (newPos, saleResults)
        } else if (ord.operationType == "sell") {
          val (newPos, newSales): (Position, Iterable[SaleResult]) = pos.sell(ord.quantity.toInt, ord.price, ord.datetime)
          (newPos, newSales ++ saleResults)
        } else
          throw new IllegalArgumentException(
            "Trying to build a Position from an order that is neither buy nor sell"
          )
      }
  }
}
