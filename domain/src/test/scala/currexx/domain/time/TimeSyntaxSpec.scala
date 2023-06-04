package currexx.domain.time

import currexx.domain.time.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.temporal.ChronoUnit
import java.time.{DayOfWeek, Instant, LocalDate}
import scala.concurrent.duration.*

class TimeSyntaxSpec extends AnyWordSpec with Matchers {

  val ts = Instant.parse("2020-01-01T00:00:00Z")

  "A String extension" should {
    "convert str to instant" in {
      "2020-01-01".toInstant mustBe Right(ts)
      "2020-01-01T00:00:00".toInstant mustBe Right(ts)
      "2020-01-01T00:00:00Z".toInstant mustBe Right(ts)
      "foo".toInstant.left.map(_.getMessage) mustBe Left("Text 'foo' could not be parsed at index 0")
    }
  }

  "A LocalDate extension" should {
    "convert ld to instant" in {
      LocalDate.parse("2020-01-01").toInstantAtStartOfDay mustBe ts
    }
  }

  "A FiniteDuration extension" should {
    "convert fd to readable string" in {
      100.millis.toReadableString mustBe "0s"
      30.minutes.toReadableString mustBe "30m"
      60.minutes.toReadableString mustBe "1h"
      90.minutes.toReadableString mustBe "1h30m"
      27.hours.toReadableString mustBe "1d3h"
    }
  }

  "An Instant extension" should {
    "return true when 2 instances have same dates" in {
      ts.hasSameDateAs(ts.plusSeconds(3600L)) mustBe true
      ts.hasSameDateAs(Instant.now) mustBe false
    }

    "return current hour" in {
      ts.hour mustBe 0
    }

    "return day of week" in {
      ts.dayOfWeek mustBe DayOfWeek.WEDNESDAY
    }

    "change instant time" in {
      ts.plusSeconds(3600L).atStartOfDay mustBe ts
      ts.plusSeconds(3600L).atEndOfDay mustBe ts.plus(1, ChronoUnit.DAYS).minusSeconds(1)
    }

    "return duration between 2 instances" in {
      ts.durationBetween(ts.plusSeconds(3600L)) mustBe 1.hour
    }
  }

}
