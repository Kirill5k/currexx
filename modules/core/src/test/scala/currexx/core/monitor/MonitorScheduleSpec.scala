package currexx.core.monitor

import currexx.core.fixtures.Monitors
import currexx.domain.monitor.Schedule
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MonitorScheduleSpec extends AnyWordSpec with Matchers {

  val now = Instant.parse("2022-01-01T00:00:00Z")

  "A Monitor" should {

    "return duration between next period for periodic schedule without last query date" in {
      val mon = Monitors.genMarketData(schedule = Schedule.Periodic(2.minutes), lastQueriedAt = None)

      mon.durationBetweenNextQuery(now) mustBe 0.seconds
    }

    "return duration between next period for periodic schedule with last query date" in {
      val mon = Monitors.genMarketData(schedule = Schedule.Periodic(2.minutes), lastQueriedAt = Some(now.minusSeconds(60)))

      mon.durationBetweenNextQuery(now) mustBe 1.minute
    }

    "return duration between next period for cron schedule without last query date" in {
      val mon = Monitors.genMarketData(schedule = Schedule.Cron.unsafe("0 7,20 * * 1-5"), lastQueriedAt = None)

      mon.durationBetweenNextQuery(now) mustBe 2.days + 7.hours
    }

    "return duration between next period for cron schedule with last query date" in {
      val mon = Monitors.genMarketData(
        schedule = Schedule.Cron.unsafe("0 7,20 * * 1-5"),
        lastQueriedAt = Some(now.minusSeconds(1.day.toSeconds))
      )

      mon.durationBetweenNextQuery(now) mustBe 2.days + 7.hours
    }
  }
}
