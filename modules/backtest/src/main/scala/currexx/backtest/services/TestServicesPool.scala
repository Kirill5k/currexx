package currexx.backtest.services

import cats.effect.Temporal
import cats.effect.std.Queue
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.TestSettings

/**
 * A pool of reusable TestServices instances to avoid the overhead of creating new services
 * for each evaluation during genetic algorithm optimization.
 *
 * @param available Queue of available TestServices ready to be acquired
 * @param poolSize Total size of the pool
 */
final class TestServicesPool[F[_]] private (
    private val available: Queue[F, TestServices[F]],
    val poolSize: Int
)(using F: Temporal[F]) {

  /**
   * Acquire a TestServices instance from the pool, reset it with new settings,
   * and return it ready for use.
   *
   * This method blocks if all services are currently in use.
   */
  private def acquire(settings: TestSettings): F[TestServices[F]] =
    for
      service <- available.take
      _       <- service.reset(settings)
    yield service

  /**
   * Release a TestServices instance back to the pool for reuse.
   */
  private def release(service: TestServices[F]): F[Unit] =
    available.offer(service)

  /**
   * Acquire a service, use it for a computation, and automatically release it back to the pool.
   *
   * This is the recommended way to use the pool as it ensures services are always returned.
   */
  def use[A](settings: TestSettings)(f: TestServices[F] => F[A]): F[A] =
    for
      service <- acquire(settings)
      result  <- f(service).attempt
      _       <- release(service)
      res     <- F.fromEither(result)
    yield res
}

object TestServicesPool {

  /**
   * Create a new pool of TestServices instances.
   *
   * @param initialSettings Settings to initialize the services with
   * @param poolSize Number of services to maintain in the pool (should match parallelism level)
   */
  def make[F[_]: Temporal](
      initialSettings: TestSettings,
      poolSize: Int
  ): F[TestServicesPool[F]] =
    for
      queue    <- Queue.bounded[F, TestServices[F]](poolSize)
      services <- List.fill(poolSize)(TestServices.make[F](initialSettings)).sequence
      _        <- services.traverse(queue.offer)
    yield TestServicesPool[F](queue, poolSize)
}
