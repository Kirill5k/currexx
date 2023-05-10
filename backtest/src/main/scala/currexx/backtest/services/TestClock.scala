package currexx.backtest.services

import cats.effect.{Ref, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.domain.time.Clock
import currexx.domain.time.syntax.*

import java.time.Instant
import scala.concurrent.duration.*

final class TestClock[F[_]](
    private val time: Ref[F, Option[Instant]]
)(using
    F: Temporal[F]
) extends Clock[F]:
  def setTime(newTime: Instant): F[Unit] =
    time.set(Some(newTime))

  override def durationBetweenNowAnd(otherTs: Instant): F[FiniteDuration] =
    now.map(_.durationBetween(otherTs))

  override def sleep(duration: FiniteDuration): F[Unit] =
    F.unit

  override def now: F[Instant] =
    time.get.flatMap {
      case Some(t) => F.pure(t)
      case None    => F.realTimeInstant
    }

object TestClock:
  def make[F[_]: Temporal]: F[TestClock[F]] = Ref.of(Option.empty[Instant]).map(TestClock[F](_))
