package daterangescala

import scala._

case class DateRange(start: Date, end: Date) {
  require(start.rank < end.rank, "Start date must be less than end date")

  def daysBetween: Int = end.rank - start.rank + 1

  def monthsBetween: Double = {
    val startRankByMonth = start.rankByMonth
    val endRankByMonth = end.rankByMonth

    val fullMonthsBetween = endRankByMonth._1 - startRankByMonth._1
    val daysDifference = endRankByMonth._2 - startRankByMonth._2

    val partialMonths = daysDifference.toDouble/Date.daysInMonth(end.y, end.m)

    fullMonthsBetween + round(partialMonths)
  }

  def isInRange(d: Date) = {
    val r = d.rank
    r >= start.rank && r <= end.rank
  }

  private def round(x: Double): Double = {
    BigDecimal(x).setScale(1, BigDecimal.RoundingMode.HALF_UP).toDouble
  }

}
