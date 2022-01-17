package com.maurogm.investments.util

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec

object Finance {
  private val SECONDS_IN_YEAR = 3600 * 24 * 365.25

  /**
   * @param returnRate 0 for break-even, 1 for a 100% gain, -0.5 if half was lost.
   */
  def annualReturnRate(
      returnRate: BigDecimal,
      investmentDurationInYears: Double
  ): Double = {
    math.pow(1 + returnRate.toDouble, 1 / investmentDurationInYears)
  }

  def annualReturnRate(
      returnRate: BigDecimal,
      investmentStart: LocalDateTime,
      investmentEnd: LocalDateTime
  ): Double = {
    val durationInSeconds =
      ChronoUnit.SECONDS.between(investmentStart, investmentEnd)
    val durationInYears = durationInSeconds / SECONDS_IN_YEAR
    annualReturnRate(returnRate, durationInYears)
  }
}
