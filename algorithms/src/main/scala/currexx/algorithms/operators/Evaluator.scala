package currexx.algorithms.operators

import cats.effect.{Concurrent, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.algorithms.Fitness

trait Evaluator[F[_], I]:
  def evaluateIndividual(individual: I): F[(I, Fitness)]

object Evaluator:
  def cached[F[_]: Concurrent, I](evaluate: I => F[(I, Fitness)]): F[Evaluator[F, I]] =
    Ref.of(Map.empty[I, Fitness]).map { cache =>
      new Evaluator[F, I] {
        override def evaluateIndividual(individual: I): F[(I, Fitness)] =
          cache.get.flatMap { storedResult =>
            storedResult.get(individual) match
              case Some(fitness) => Concurrent[F].pure((individual, fitness))
              case None          => evaluate(individual).flatTap(res => cache.update(_ + res))
          }
      }
    }
