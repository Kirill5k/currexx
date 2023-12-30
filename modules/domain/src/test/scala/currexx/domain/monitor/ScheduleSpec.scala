package currexx.domain.monitor

import org.scalatest.EitherValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import io.circe.parser.*
import io.circe.syntax.*

import java.time.Instant
import scala.concurrent.duration.*

class ScheduleSpec extends AnyWordSpec with Matchers with EitherValues {

  val ts = Instant.parse("2022-01-01T00:00:00Z") // Saturday

  "A Schedule" when {
    "Periodic" should {
      "return next execution time based on period" in {
        val cron = Schedule.Periodic(5.hours)

        cron.nextExecutionTime(ts) mustBe Instant.parse("2022-01-01T05:00:00Z")
      }

      "encode and decode into json" in {
        val schedule: Schedule = Schedule.Periodic(5.hours)
        val json               = schedule.asJson.noSpaces

        decode[Schedule](json) mustBe Right(schedule)
        json mustBe """{"kind":"periodic","period":"5 hours"}"""
      }
    }

    "Cron" should {
      "return next execution time based on cron schedule" in {
        val cron = Schedule.Cron("0 7,20 * * 1-5").value // every monday-friday at 7:00 and 20:00 UTC

        cron.nextExecutionTime(ts) mustBe Instant.parse("2022-01-03T07:00:00Z")
      }

      "encode and decode into json" in {
        val schedule: Schedule = Schedule.Cron("0 7,20 * * 1-5").value
        val json               = schedule.asJson.noSpaces

        decode[Schedule](json) mustBe a[Right[_, _]]
        json mustBe """{"kind":"cron","cron":"0 7,20 * * 1-5"}"""
      }
    }
  }
}
