package wilson

object Date {
  val thirtyDays = List(4, 6, 9, 11)

  def daysInMonth(y: Int, m: Int): Int = {
    (y, m) match {
      case (y, 2) if isLeap(y) => 29
      case (_, 2) => 28
      case (_, m) if (thirtyDays contains m) => 30
      case (_, _) => 31
    }
  }

  def isLeap(y: Int) = y % 4 == 0 && (y % 100 != 0 || y % 400 == 0)
}

case class Date(y: Int, m: Int, d: Int) {
  require(y > 0, "Year must be greater than 1")
  require(m > 0, "Month must be greater than 1")
  require(m <= 12, "Month must be less than or equal 12")
  require(d > 0, "Day must be greater than 1")
  require(d <= Date.daysInMonth(y, m), "Day is invalid")

  def rank: Int = {
    val yearsPast = y - 1
    val leapYearsPast = yearsPast / 4 - yearsPast / 100 + yearsPast / 400
    val daysToStartOfYear = 365 * yearsPast + leapYearsPast
    val daysFromStartOfYear = d + (1 until m).map(Date.daysInMonth(y, _)).sum
    (daysToStartOfYear + daysFromStartOfYear)
  }

  def rankByMonth: (Int, Int) = {
    val yearsPast = y - 1
    val fullMonthsPast = yearsPast * 12 + m - 1
    val partialMonth = d
    (fullMonthsPast, partialMonth)
  }

  override def toString = f"$y%04d/$m%02d/$d%02d"
}

