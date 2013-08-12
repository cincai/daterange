package daterangescala

import org.junit.runner.RunWith
import org.scalatest._
import org.scalatest.junit.JUnitRunner
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.prop.TableDrivenPropertyChecks

@RunWith(classOf[JUnitRunner])
class DateSpec extends FunSuite with TableDrivenPropertyChecks with ShouldMatchers {
  val validYear = 1

  val exampleLeapYears = Table("Leap years", 4, 40, 400, 2000)
  val exampleNonLeapYears = Table("Non-leap years", 1, 41, 100, 2001)

  val exampleInvalidMonths = Table("Invalid months", 0, 13)

  val allMonths = Table("All months", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)

  val thirtyDays = Table("Thirty-day months", 4, 6, 9, 11)

  test("Year can be equal or greater than 1") {
    forAll(exampleNonLeapYears) { y =>
      val d = Date(y, 1, 1)
      d.y should be(y)
      d.m should be(1)
      d.d should be(1)
    }
  }

  test("Year cannot be less than 1") {
    intercept[IllegalArgumentException] {
      Date(0, 1, 1)
    }
  }

  test("Month can be between 1 and 12") {
    forAll(allMonths) { m =>
      val d = Date(validYear, m, 1)
      d.y should be(validYear)
      d.m should be(m)
      d.d should be(1)
    }
  }

  test("Month must be between 1 and 12") {
    forAll(exampleInvalidMonths) { m =>
      intercept[IllegalArgumentException] {
        Date(validYear, m, 1)
      }
    }
  }

  test("Day cannot be less than 1 for all months") {
    forAll(allMonths) { m =>
      intercept[IllegalArgumentException] {
        Date(validYear, m, 0)
      }
    }
  }


  test("Day cannot be more than 31 for all months") {
    forAll(allMonths) { m =>
      intercept[IllegalArgumentException] {
        Date(validYear, m, 32)
      }
    }
  }

  test("Day in February can be 29 for leap years") {
    forAll(exampleLeapYears) { y =>
      val d = Date(y, 2, 29)
      d.y should be(y)
      d.m should be(2)
      d.d should be(29)
    }
  }

  test("Day in February can be 28 for non-leap years") {
    forAll(exampleNonLeapYears) { y =>
      val d = Date(y, 2, 28)
      d.y should be(y)
      d.m should be(2)
      d.d should be(28)
    }
  }

  test("Day in February cannot be 29 for non-leap years") {
    forAll(exampleNonLeapYears) { y =>
      intercept[IllegalArgumentException] {
        Date(y, 2, 29)
      }
    }
  }

  test("Day cannot be 31 for thirty-day months") {
    forAll(thirtyDays) { m =>
      intercept[IllegalArgumentException] {
        Date(validYear, m, 31)
      }
    }
  }

  test("Rank 0001/01/01 should be 1") {
    Date(1, 1, 1).rank should equal(1)
  }

  test("Rank 0001/12/31 should be 365") {
    Date(1, 12, 31).rank should equal(365)
  }

  test("Rank 0002/01/01 should be 366") {
    Date(2, 1, 1).rank should equal(366)
  }

  test("Rank 0004/12/31 should be 3 * 365 + 366") {
    Date(4, 12, 31).rank should equal(1461)
  }

  test("Rank 0100/12/31 should be 76 * 365 + 24 * 366") {
    Date(100, 12, 31).rank should equal(36524)
  }

  test("Rank 0400/12/31 should be 3 * (76 * 365 + 24 * 366) + (75 * 365 + 25 * 366)") {
    Date(400, 12, 31).rank should equal(146097)
  }

  test("Rank By Month 0001/01/15 should be (0, 15)") {
    Date(1, 1, 15).rankByMonth should equal((0, 15))
  }

  test("Rank By Month 0001/01/31 should be (0, 31)") {
    Date(1, 1, 31).rankByMonth should equal((0, 31))
  }

}
