package currexx.core

import cats.Monad
import currexx.domain.time.Clock

import java.time.Instant

final private class MockClock[F[_]](
    private val time: Instant
)(using F: Monad[F])
    extends Clock[F]:
  override def currentTime: F[Instant] = F.pure(time)

object MockClock:
  def apply[F[_]: Monad](currentTime: Instant): Clock[F] = new MockClock[F](currentTime)
