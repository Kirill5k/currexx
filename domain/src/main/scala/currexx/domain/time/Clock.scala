package currexx.domain.time

import cats.effect.Temporal

import java.time.Instant

trait Clock[F[_]]:
  def currentTime: F[Instant]

final private class LiveClock[F[_]](using F: Temporal[F]) extends Clock[F]:
  override def currentTime: F[Instant] = F.realTimeInstant
  
object Clock:
  def default[F[_]: Temporal]: Clock[F] = LiveClock[F]