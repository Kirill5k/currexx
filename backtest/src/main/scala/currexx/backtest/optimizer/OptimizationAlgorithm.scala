package currexx.backtest.optimizer

import cats.effect.Async
import currexx.algorithms.operators.*
import currexx.algorithms.{Alg, Fitness, Parameters}

import scala.reflect.ClassTag
import scala.util.Random

trait OptimizationAlgorithm[F[_], A <: Alg, T]:
  def optimize(
      target: T,
      params: Parameters[A]
  )(using
      rand: Random
  ): F[(T, Fitness)]

object OptimizationAlgorithm {
  inline def geneticAlgorithm[F[_]: Async, T, A: ClassTag](
      crossover: Crossover[A],
      mutator: Mutator[A],
      evaluator: Evaluator[A],
      selector: Selector[A],
      elitism: Elitism[A]
  ): OptimizationAlgorithm[F, Alg.GA, T] = ???
}
