package currexx.algorithms.operators

import cats.effect.{Concurrent, Deferred, Ref}
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.algorithms.Fitness

trait Evaluator[F[_], I]:
  def evaluateIndividual(individual: I): F[(I, Fitness)]

object Evaluator:
  def cached[F[_]: Concurrent, I](objectiveFn: I => F[(I, Fitness)]): F[Evaluator[F, I]] =
    Ref.of(Map.empty[I, Deferred[F, Either[Throwable, Fitness]]]).map { cache =>
      new Evaluator[F, I]:
        override def evaluateIndividual(individual: I): F[(I, Fitness)] =
          cache.get.flatMap(_.get(individual) match
            case Some(d) => await(d, individual)
            case None    => computeOrWait(individual)
          )

        private def await(d: Deferred[F, Either[Throwable, Fitness]], individual: I): F[(I, Fitness)] =
          d.get.flatMap {
            case Right(fitness) => Concurrent[F].pure((individual, fitness))
            case Left(err)      => Concurrent[F].raiseError(err)
          }

        private def computeOrWait(individual: I): F[(I, Fitness)] =
          Deferred[F, Either[Throwable, Fitness]].flatMap { newDeferred =>
            cache.modify { map =>
              map.get(individual) match
                case Some(existing) => (map, existing)
                case None           => (map + (individual -> newDeferred), newDeferred)
            }.flatMap { winner =>
              if (winner eq newDeferred) compute(individual, newDeferred)
              else await(winner, individual)
            }
          }

        private def compute(individual: I, slot: Deferred[F, Either[Throwable, Fitness]]): F[(I, Fitness)] =
          objectiveFn(individual)
            .attempt
            .map(_.map(_._2))
            .flatTap { result =>
              slot.complete(result) >>
                (if result.isLeft then cache.update(_ - individual) else Concurrent[F].pure(()))
            }
            .flatMap {
              case Right(fitness) => Concurrent[F].pure((individual, fitness))
              case Left(err)      => Concurrent[F].raiseError(err)
            }
    }
