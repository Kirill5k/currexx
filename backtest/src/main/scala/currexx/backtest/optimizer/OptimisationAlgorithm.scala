package currexx.backtest.optimizer

import cats.effect.Async
import cats.syntax.functor.*
import currexx.algorithms.operators.*
import currexx.algorithms.{Alg, Algorithm, EvaluatedPopulation, Op, Parameters}

import scala.reflect.ClassTag
import scala.util.Random

trait OptimisationAlgorithm[F[_], A <: Alg, P <: Parameters[A], T]:
  def optimise(target: T, params: P)(using rand: Random): F[EvaluatedPopulation[T]]

object OptimisationAlgorithm:
  def ga[F[_]: Async, T](
      initialiser: Initialiser[F, T],
      crossover: Crossover[F, T],
      mutator: Mutator[F, T],
      evaluator: Evaluator[F, T],
      selector: Selector[F, T],
      elitism: Elitism[F, T],
      updateFn: (Int, Int) => F[Unit]
  ): OptimisationAlgorithm[F, Alg.GA, Parameters.GA, T] = new OptimisationAlgorithm[F, Alg.GA, Parameters.GA, T]:
    override def optimise(target: T, params: Parameters.GA)(using rand: Random): F[EvaluatedPopulation[T]] =
      Algorithm.GA
        .optimise[T](target, params)
        .foldMap(Op.ioInterpreter[F, T](initialiser, crossover, mutator, evaluator, selector, elitism, Some(updateFn)))
