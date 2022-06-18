package currexx.algorithms.operators

import cats.Show
import cats.effect.{Concurrent, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.algorithms.Fitness

trait Evaluator[F[_], I]:
  def evaluateIndividual(individual: I): F[(I, Fitness)]

object Evaluator:
  def cached[F[_]: Concurrent, I: Show](objectiveFn: I => F[(I, Fitness)]): F[Evaluator[F, I]] =
    Ref.of(Map.empty[String, Fitness]).map { cache =>
      new Evaluator[F, I] {
        override def evaluateIndividual(individual: I): F[(I, Fitness)] =
          cache.get.flatMap { storedResult =>
            val stringRepr = Show[I].show(individual)
            storedResult.get(stringRepr) match
              case Some(fitness) => Concurrent[F].pure((individual, fitness))
              case None          => objectiveFn(individual).flatTap((_, f) => cache.update(_ + (stringRepr -> f)))
          }
      }
    }
