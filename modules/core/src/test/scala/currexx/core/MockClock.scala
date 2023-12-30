package currexx.core

import cats.Monad
import currexx.domain.time.Clock
import currexx.domain.time.syntax.*

import java.time.Instant
import scala.concurrent.duration.*

final private class MockClock[F[_]](
    private var time: Instant
)(using
    F: Monad[F]
) extends Clock[F] {
  override def durationBetweenNowAnd(otherTime: Instant): F[FiniteDuration] =
    F.pure(time.durationBetween(otherTime))

  override def sleep(duration: FiniteDuration): F[Unit] = {
    time = time.plusNanos(duration.toNanos)
    F.pure(())
  }

  override def now: F[Instant] = F.pure(time)

}

object MockClock:
  def apply[F[_]: Monad](currentTime: Instant): Clock[F] = new MockClock[F](currentTime)
