package currexx.domain.time

import java.time.{DayOfWeek, Instant, LocalDate, ZoneOffset}
import scala.concurrent.duration.*
import scala.util.Try

object syntax {
  extension (dateString: String)
    def toInstant: Either[Throwable, Instant] =
      val localDate = dateString.length match
        case 10 => s"${dateString}T00:00:00Z"
        case 19 => s"${dateString}Z"
        case _  => dateString
      Try(Instant.parse(localDate)).toEither

  extension (ld: LocalDate) def toInstantAtStartOfDay: Instant = ld.atStartOfDay().toInstant(ZoneOffset.UTC)
  extension (ts: Instant)
    def minus(duration: FiniteDuration): Instant          = ts.minusNanos(duration.toNanos)
    def plus(duration: FiniteDuration): Instant           = ts.plusNanos(duration.toNanos)
    def hour: Int                                         = ts.atZone(ZoneOffset.UTC).getHour
    def dayOfWeek: DayOfWeek                              = ts.atZone(ZoneOffset.UTC).getDayOfWeek
    def toLocalDate: LocalDate                            = LocalDate.parse(ts.toString.slice(0, 10))
    def atStartOfDay: Instant                             = toLocalDate.atStartOfDay().toInstant(ZoneOffset.UTC)
    def atEndOfDay: Instant                               = toLocalDate.plusDays(1).atStartOfDay().minusSeconds(1).toInstant(ZoneOffset.UTC)
    def durationBetween(otherTs: Instant): FiniteDuration = math.abs(otherTs.toEpochMilli - ts.toEpochMilli).millis
    def hasSameDateAs(otherTs: Instant): Boolean          = ts.toString.slice(0, 10) == otherTs.toString.slice(0, 10)

  extension (fd: FiniteDuration)
    def toReadableString: String =
      val days     = fd.toDays
      val remHours = fd - days.days
      val hours    = remHours.toHours
      val remMins  = remHours - hours.hours
      val minutes  = remMins.toMinutes
      val remSecs  = remMins - minutes.minutes
      val seconds  = remSecs.toSeconds
      val result =
        s"""
           |${if days > 0 then s"${days}d" else ""}
           |${if hours > 0 then s"${hours}h" else ""}
           |${if minutes > 0 then s"${minutes}m" else ""}
           |${if seconds > 0 then s"${seconds}s" else ""}
           |""".stripMargin.strip.replaceAll("\n", "")
      if result == "" then "0s" else result
}
