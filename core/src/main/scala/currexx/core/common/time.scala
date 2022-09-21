package currexx.core.common

import java.time.{Instant, LocalDate, LocalDateTime, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.Try

object time:
  extension (dateString: String)
    def toInstant: Either[Throwable, Instant] =
      val localDate = dateString.length match
        case 10 => s"${dateString}T00:00:00Z"
        case 19 => s"${dateString}Z"
        case _  => dateString
      Try(Instant.parse(localDate)).toEither

  extension (ld: LocalDate) def toInstantAtStartOfDay: Instant = ld.atStartOfDay().toInstant(ZoneOffset.UTC)
  extension (ts: Instant)
    def toLocalDate: LocalDate = LocalDate.parse(ts.toString.slice(0, 10))
    def atStartOfDay: Instant  = toLocalDate.atStartOfDay().toInstant(ZoneOffset.UTC)
    def atEndOfDay: Instant    = toLocalDate.plusDays(1).atStartOfDay().minusSeconds(1).toInstant(ZoneOffset.UTC)
    def durationBetween(otherTs: Instant): FiniteDuration =
      math.abs(otherTs.toEpochMilli - ts.toEpochMilli).millis
    def hasSameDateAs(otherTs: Instant): Boolean =
      ts.toString.slice(0, 10) == otherTs.toString.slice(0, 10)

  extension (fd: FiniteDuration)
    def toReadableString: String = {
      val hours   = fd.toHours
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
