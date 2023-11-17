package com.maurogm.investments.persistence

import com.maurogm.investments.{
  Activity,
  Asset,
  Movement,
  Order,
  Portfolio,
  PositionFraction
}
import com.maurogm.investments.currency.{CurrencyConverter, Money}
import com.maurogm.investments.etl.util.CSVSerializer
import com.maurogm.investments.etl.util.Utils.writeAsCsv
import com.maurogm.investments.etl.market.AssetExtension.*

import java.time.{LocalDate, LocalDateTime}

object ActivityPersistence {

  def persistAsCSV(
      activity: Activity,
      filePathsRoot: String = "src/main/resources/outputs/",
      movementsFileName: String = "unified_movements.csv",
      ordersFileName: String = "unified_orders.csv",
      positionFileName: String = "current_position.csv",
      recentPricesFileName: String = "recent_prices.csv"
  )(using
      cc: CurrencyConverter
  ): Unit = {
    movementsToCSV(
      activity.movements.sortBy(_.date),
      filePath = filePathsRoot + movementsFileName
    )
    ordersToCSV(activity.orders.sortBy(_.datetime), filePath = filePathsRoot + ordersFileName)
    positionsToCSV(
      activity.currentPortfolio,
      filePath = filePathsRoot + positionFileName
    )
    assetPricesToCSV(
      activity.getTradedAssets,
      filePath = filePathsRoot + recentPricesFileName
    )
  }

  private def movementsToCSV(movements: Seq[Movement], filePath: String): Unit =
    val headers =
      Seq("broker", "date", "movementType", "ticker", "currency", "amount")
    writeAsCsv(
      data = movements,
      filePath = filePath,
      append = false,
      headers = Option(headers)
    )

  private def ordersToCSV(orders: Seq[Order], filePath: String): Unit =
    val headers = Seq(
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
    writeAsCsv(
      data = orders,
      filePath = filePath,
      append = false,
      headers = Option(headers)
    )

  private def positionsToCSV(portfolio: Portfolio, filePath: String): Unit = {
    val headers = Seq(
      "exchange",
      "ticker",
      "datetimeOpening",
      "currency",
      "pricePayedAmount",
      "remainingQuantity"
    )
    val unzippedPositionFractions: Iterable[UnzippedPositionFraction] = for {
      (asset, position) <- portfolio
      posFraction <- position.breakdown
      PositionFraction(datetimeOpening, pricePayed, remainingQuantity) =
        posFraction
    } yield UnzippedPositionFraction(
      asset,
      datetimeOpening,
      pricePayed,
      remainingQuantity
    )
    writeAsCsv(
      data = unzippedPositionFractions.toSeq,
      filePath = filePath,
      append = false,
      headers = Option(headers)
    )
  }

  private def assetPricesToCSV(assets: Iterable[Asset], filePath: String)(using
      cc: CurrencyConverter
  ): Unit =
    val headers = Seq(
      "exchange",
      "ticker",
      "date",
      "localCurrency",
      "priceLocalCurrency",
      "homogeneousCurrency",
      "priceHomogeneousCurrency"
    )
    val assetsRecentPrices = assets
      .map(asset =>
        AssetPrice(
          asset,
          asset.getMostRecentDateWithPrice,
          asset.getMostRecentPrice,
          asset.getMostRecentPriceHomogeneous
        )
      )
    writeAsCsv(
      data = assetsRecentPrices.toSeq,
      filePath = filePath,
      append = false,
      headers = Option(headers)
    )
}

case class UnzippedPositionFraction(
    asset: Asset,
    datetimeOpening: LocalDateTime,
    pricePayed: Money,
    remainingQuantity: BigDecimal
) extends CSVSerializer {
  override def toCsv: String = {
    s"${this.asset.exchange},${this.asset.ticker},${this.datetimeOpening},${this.pricePayed.currency},${this.pricePayed.amount},${this.remainingQuantity}"
  }
}

case class AssetPrice(
    asset: Asset,
    maybeDate: Either[String, LocalDate],
    maybePriceOriginal: Either[String, Money],
    maybePriceHomogeneous: Either[String, Money]
) extends CSVSerializer {
  override def toCsv: String = {
    val dateStr: String = maybeDate match
      case Left(_)     => ""
      case Right(date) => date.toString
    def maybePriceToStr(maybePrice: Either[String, Money]): String =
      maybePrice match
        case Left(_)                        => ","
        case Right(Money(currency, amount)) => s"$currency,$amount"
    val priceStrOriginal = maybePriceToStr(maybePriceOriginal)
    val priceStrHomogeneous = maybePriceToStr(maybePriceHomogeneous)
    s"${asset.exchange},${asset.ticker},$dateStr,$priceStrOriginal,$priceStrHomogeneous"
  }
}
