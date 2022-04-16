package currexx.core.common

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.Try

object time:
  extension (ts: Instant)
    def durationBetween(otherTs: Instant): FiniteDuration =
      math.abs(ts.toEpochMilli - otherTs.toEpochMilli).millis

  extension (fd: FiniteDuration)
    def toReadableString: String = {
      val hours = fd.toHours
      val remMins = fd - hours.hours
      val minutes = remMins.toMinutes
      val remSecs = remMins - minutes.minutes
      val seconds = remSecs.toSeconds
      s"""
         |${if hours > 0 then s"${hours}h" else ""}
         |${if minutes > 0 then s"${minutes}m" else ""}
         |${if seconds > 0 then s"${seconds}s" else ""}
         |""".stripMargin.replaceAll("\n", "")
    }
