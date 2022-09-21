package currexx.core.monitor

import currexx.domain.market.Interval
import currexx.domain.monitor.Schedule
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MonitorScheduleSpec extends AnyWordSpec with Matchers {

  val now = Instant.parse("2022-01-01T00:00:00Z")

  "A PriceMonitorSchedule" should {

    "return duration between next period for periodic schedule without last query date" in {
      val pms = PriceMonitorSchedule(Interval.D1, Schedule.Periodic(2.minutes), None)

      pms.durationBetweenNextQuery(now) mustBe 0.seconds
    }

    "return duration between next period for periodic schedule with last query date" in {
      val pms = PriceMonitorSchedule(Interval.D1, Schedule.Periodic(2.minutes), Some(now.minusSeconds(60)))

      pms.durationBetweenNextQuery(now) mustBe 1.minute
    }

    "return duration between next period for cron schedule without last query date" in {
      val pms = PriceMonitorSchedule(Interval.D1, Schedule.Cron.unsafe("0 7,20 * * 1-5"), None)

      pms.durationBetweenNextQuery(now) mustBe 2.days + 7.hours
    }

    "return duration between next period for cron schedule with last query date" in {
      val pms = PriceMonitorSchedule(Interval.D1, Schedule.Cron.unsafe("0 7,20 * * 1-5"), Some(now.minusSeconds(1.day.toSeconds)))

      pms.durationBetweenNextQuery(now) mustBe 2.days + 7.hours
    }
  }
}
