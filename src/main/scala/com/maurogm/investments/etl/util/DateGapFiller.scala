package com.maurogm.investments.etl.util

import java.time.LocalDateTime
import java.time.temporal.ChronoUnit

object DateGapFiller {

  /** @tparam T
    *   A type with an ordering.
    * @return
    *   An ordered Sequence of tuples containing the original elements with
    *   their closest next element. The greatest element of the original
    *   sequence is mapped to a tuple with itself.
    */
  def consecutiveMap[T: Ordering](xs: Seq[T]): Seq[(T, T)] = {
    xs.sorted.foldRight[Seq[(T, T)]](Nil) {
      case (elem, Nil) => Seq((elem, elem))
      case (elem, xs)  => (elem, xs.head._1) +: xs
    }
  }

  def consecutiveFillOfDates[T](
      consecutiveTuple: (
          T with DateTimeAccessor[T],
          T with DateTimeAccessor[T]
      )
  ): Seq[T] = {
    val (firstDate, lastDate) =
      (consecutiveTuple._1.getDateTime, consecutiveTuple._2.getDateTime)
    val nDaysBetween =
      firstDate.toLocalDate.until(lastDate.toLocalDate, ChronoUnit.DAYS)
    consecutiveTuple match {
      case (a, b) if a == b => Seq(a) // Handles duplicates
      case (a, b) if nDaysBetween == 0 =>
        Seq() // Throws away intra-day data, if any
      case (a, b) if nDaysBetween > 0 => {
        val dates =
          for (i <- 0 until nDaysBetween.toInt) yield firstDate.plusDays(i)
        dates.map(date => consecutiveTuple._1.setDateTime(date))
      }
      case _ =>
        throw new IllegalArgumentException(
          s"$consecutiveTuple has a date difference of $nDaysBetween, when it should be positive"
        )
    }
  }
}

trait DateTimeAccessor[T] {
  def getDateTime: LocalDateTime
  def setDateTime(newDateTime: LocalDateTime): T
}
