package currexx.backtest.optimizer

import cats.effect.Async
import cats.syntax.functor.*
import currexx.algorithms.operators.*
import currexx.algorithms.{Alg, Algorithm, Fitness, Op, Parameters}

import scala.reflect.ClassTag
import scala.util.Random

trait OptimisationAlgorithm[F[_], A <: Alg, P <: Parameters[A]]:
  def optimise[T](target: T, params: P)(using rand: Random, opt: Optimisable[T]): F[(T, Fitness)]

object OptimisationAlgorithm:
  def ga[F[_]: Async](
      initialiser: Initialiser[F, Array[Array[Int]]],
      crossover: Crossover[F, Array[Array[Int]]],
      mutator: Mutator[F, Array[Array[Int]]],
      evaluator: Evaluator[F, Array[Array[Int]]],
      selector: Selector[F, Array[Array[Int]]],
      elitism: Elitism[F, Array[Array[Int]]]
  ): OptimisationAlgorithm[F, Alg.GA, Parameters.GA] = new OptimisationAlgorithm[F, Alg.GA, Parameters.GA]:
    override def optimise[T](target: T, params: Parameters.GA)(using rand: Random, opt: Optimisable[T]): F[(T, Fitness)] =
      Algorithm.GA
        .optimise[Array[Array[Int]]](opt.toGenome(target), params)
        .foldMap(Op.ioInterpreter[F, Array[Array[Int]]](initialiser, crossover, mutator, evaluator, selector, elitism, None))
        .map((res, fit) => (opt.fromGenome(res), fit))
