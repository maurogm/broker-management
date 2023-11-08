package com.maurogm.investments

import com.maurogm.investments.currency.Money
import com.maurogm.investments.etl.util.CSVSerializer
import com.maurogm.investments.util.Finance.annualReturnRate

import java.time.LocalDateTime

case class PositionFraction(
    datetimeOpening: LocalDateTime,
    pricePayed: Money,
    remainingQuantity: BigDecimal
)

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

case class SaleResult(
    dateOpening: LocalDateTime,
    pricePayed: Money,
    soldQuantity: BigDecimal,
    profitOrLoss: Money,
    profitRatio: BigDecimal,
    profitRatioAnnualized: Double
)

/** Records the current position in an Asset, while keeping a record of each
  * date the position was increased (and the price paid each instance).
  *
  * When the position is reduced, it is assumed that the sold assets come from
  * all instances proportionally to their current volume.
  */
case class Position(breakdown: Seq[PositionFraction]) {
  lazy val total: BigDecimal =
    breakdown.foldLeft(BigDecimal(0))(_ + _.remainingQuantity)

  def buy(n: Int, price: Money, datetime: LocalDateTime): Position = {
    val newFraction = PositionFraction(datetime, price, BigDecimal(n))
    Position(newFraction +: breakdown)
  }

  /** Reduce the position in the asset by selling at least some of the asset.
    * @param n
    *   Quantity of units of the asset to sell.
    * @param price
    *   Selling price.
    */
  def sell(
      n: Int,
      price: Money,
      datetime: LocalDateTime
  ): (Position, Iterable[SaleResult]) = {
    require(
      n <= total,
      s"Tried to sell $n units of the asset, but the position amounts to only $total"
    )
    val saleBreakdown = breakdown.map {
      case PositionFraction(dateOpening, pricePayed, remainingQuantity) =>
        val nSold = n * remainingQuantity / total
        val profitRatio = price / pricePayed
        val remainingPositionFraction =
          PositionFraction(dateOpening, pricePayed, remainingQuantity - nSold)
        val saleResult = SaleResult(
          dateOpening,
          pricePayed,
          nSold,
          (price - pricePayed) * nSold,
          profitRatio,
          annualReturnRate(profitRatio - 1, dateOpening, datetime)
        )
        (remainingPositionFraction, saleResult)
    }
    val newPosition = Position(saleBreakdown.map(_._1))
    val breakdownSaleResult = saleBreakdown.map(_._2)
    (newPosition, breakdownSaleResult)
  }
}
