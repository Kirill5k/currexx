package currexx.domain.cache

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.effect.syntax.spawn.*
import cats.effect.Clock
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import scala.concurrent.duration.FiniteDuration

trait Cache[F[_], K, V]:
  def get(key: K): F[Option[V]]
  def put(key: K, value: V): F[Unit]

final private class RefbasedCache[F[_]: Clock: Monad, K, V](
    private val state: Ref[F, Map[K, (V, Long)]]
) extends Cache[F, K, V] {

  override def get(key: K): F[Option[V]] =
    state.get.map(_.get(key).map(_._1))

  override def put(key: K, value: V): F[Unit] =
    for
      ts <- Clock[F].realTime
      _  <- state.update(_ + (key -> (value -> ts.toMillis)))
    yield ()
}

object Cache:
  def make[F[_], K, V](
      expiresIn: FiniteDuration,
      checkOnEvery: FiniteDuration
  )(using F: Temporal[F]): F[Cache[F, K, V]] = {
    def checkExpirations(state: Ref[F, Map[K, (V, Long)]]): F[Unit] = {
      val process = F.realTime.flatMap { ts =>
        state.update(_.filter { case (_, (_, exp)) =>
          exp + expiresIn.toMillis > ts.toMillis
        })
      }

      F.sleep(checkOnEvery) >> process >> checkExpirations(state)
    }

    Ref
      .of[F, Map[K, (V, Long)]](Map.empty[K, (V, Long)])
      .flatTap(s => checkExpirations(s).start.void)
      .map(s => RefbasedCache[F, K, V](s))
  }
