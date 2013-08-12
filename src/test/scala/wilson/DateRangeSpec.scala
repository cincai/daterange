package wilson

import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.mock.MockitoSugar
import org.mockito.Mockito.verify
import org.mockito.Mockito.when

@RunWith(classOf[JUnitRunner])
class DateRangeSpec extends FunSuite with ShouldMatchers with MockitoSugar {
  test("Integration test - Days between two dates") {
    val dr = DateRange(Date(2010, 9, 1), Date(2010, 9, 15))
    dr.daysBetween should equal(15)
  }

  test("Integration test - Months between is exactly one month apart") {
    val dr = DateRange(Date(2010, 9, 1), Date(2010, 10, 1))
    dr.monthsBetween should equal(1.0)
  }

  test("Integration test - Months between is exactly one and a half months apart") {
    val dr = DateRange(Date(2010, 9, 1), Date(2010, 10, 15))
    dr.monthsBetween should equal(1.5)
  }

  test("Integration test - Months between is half a month apart") {
    val dr = DateRange(Date(2010, 9, 1), Date(2010, 9, 15))
    dr.monthsBetween should equal(0.5)
  }

  test("Start date must be less than End date") {
    new MockRangeWithRank(1, 2) {
      val dr = DateRange(mockStart, mockEnd)
      verify(mockStart).rank
      verify(mockEnd).rank
    }
  }

  test("Start date cannot be equal End date") {
    new MockRangeWithRank(1, 1) {
      intercept[IllegalArgumentException] {
        DateRange(mockStart, mockEnd)
      }

      verify(mockStart).rank
      verify(mockEnd).rank
    }
  }

  test("Start date cannot be greater than End date") {
    new MockRangeWithRank(2, 1) {
      intercept[IllegalArgumentException] {
        DateRange(mockStart, mockEnd)
      }

      verify(mockStart).rank
      verify(mockEnd).rank
    }
  }

  test("Months between delegates to rankByMonth") {
    new MockRangeWithRankByMonth((1, 1), (2, 1)) {
      DateRange(mockStart, mockEnd).monthsBetween
      verify(mockStart).rankByMonth
      verify(mockEnd).rankByMonth
    }
  }

  test("Days between rank 1 and 10 is 10") {
    new MockRangeWithRank(1, 10) {
      val dr = DateRange(mockStart, mockEnd)
      dr.daysBetween should equal(10)
    }
  }

  test("Day 1 is in range of 1 and 10") {
    new MockRangeWithRank(1, 10) {
      val dr = DateRange(mockStart, mockEnd)
      val mockInRangeDay = mock[Date]
      when(mockInRangeDay.rank).thenReturn(1)
      dr.isInRange(mockInRangeDay) should be(true)
    }
  }

  test("Day 10 is in range of 1 and 10") {
    new MockRangeWithRank(1, 10) {
      val dr = DateRange(mockStart, mockEnd)
      val mockInRangeDay = mock[Date]
      when(mockInRangeDay.rank).thenReturn(10)
      dr.isInRange(mockInRangeDay) should be(true)
    }
  }

  test("Day 11 is not in range of 1 and 10") {
    new MockRangeWithRank(1, 10) {
      val dr = DateRange(mockStart, mockEnd)
      val mockInRangeDay = mock[Date]
      when(mockInRangeDay.rank).thenReturn(11)
      dr.isInRange(mockInRangeDay) should be(false)
    }
  }

  case class MockRangeWithRank(start: Int, end: Int) {
    val mockStart = mock[Date]
    val mockEnd = mock[Date]

    when(mockStart.rank).thenReturn(start)
    when(mockEnd.rank).thenReturn(end)
  }

  case class MockRangeWithRankByMonth(start: (Int, Int), end: (Int, Int)) {
    val mockStart = mock[Date]
    val mockEnd = mock[Date]

    when(mockStart.rank).thenReturn(1)
    when(mockEnd.rank).thenReturn(2)

    when(mockStart.rankByMonth).thenReturn(start)
    when(mockEnd.rankByMonth).thenReturn(end)
  }
}
