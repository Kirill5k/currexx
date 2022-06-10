package currexx.algorithms

import cats.~>
import cats.effect.{Async, Sync}
import cats.free.Free
import currexx.algorithms.operators.*
import fs2.Stream

import scala.reflect.ClassTag
import scala.util.Random

opaque type Fitness = BigDecimal
object Fitness:
  def apply(value: BigDecimal): Fitness = value
  extension (fitness: Fitness)
    def *(other: Int): Fitness     = fitness * other
    def <(other: Fitness): Boolean = fitness < other
    def +(other: Fitness): Fitness = fitness + other
    def -(other: Fitness): Fitness = fitness - other
    def /(other: Fitness): Fitness = fitness / other
    def value: BigDecimal          = fitness
  given ordering: Ordering[Fitness] with
    def compare(f1: Fitness, f2: Fitness) = f1.compare(f2)

type Population[I]            = Vector[I]
type EvaluatedPopulation[I]   = Vector[(I, Fitness)]
type DistributedPopulation[I] = Vector[(I, I)]

enum Op[A, I]:
  case UpdateOnProgress[I](iteration: Int, maxGen: Int)                                 extends Op[Unit, I]
  case InitPopulation[I](seed: I, size: Int, shuffle: Boolean)                          extends Op[Population[I], I]
  case Cross[I](ind1: I, ind2: I, prob: Double)                                         extends Op[Option[I], I]
  case Mutate[I](ind: I, prob: Double)                                                  extends Op[I, I]
  case EvaluateOne[I](ind: I)                                                           extends Op[(I, Fitness), I]
  case EvaluatePopulation[I](population: Population[I])                                 extends Op[EvaluatedPopulation[I], I]
  case SelectElites[I](population: EvaluatedPopulation[I], popSize: Int, ratio: Double) extends Op[Population[I], I]
  case SelectPairs[I](population: EvaluatedPopulation[I], limit: Int)                   extends Op[DistributedPopulation[I], I]
  case SelectFittest[I](population: EvaluatedPopulation[I])                             extends Op[(I, Fitness), I]
  case ApplyToAll[A, B, I](population: Population[A], op: A => Op[B, I])                extends Op[Population[B], I]

object Op:
  extension [A, I](fa: Op[A, I]) def freeM: Free[Op[*, I], A] = Free.liftF(fa)

  inline def ioInterpreter[F[_], I](
      initialiser: Initialiser[I],
      crossover: Crossover[I],
      mutator: Mutator[I],
      evaluator: Evaluator[I],
      selector: Selector[I],
      elitism: Elitism[I],
      updateFn: Option[(Int, Int) => F[Unit]] = None
  )(using F: Async[F], rand: Random): Op[*, I] ~> F = new (Op[*, I] ~> F) {
    def apply[A](fa: Op[A, I]): F[A] =
      fa match
        case Op.UpdateOnProgress(iteration, maxGen) =>
          updateFn.fold(F.unit)(f => f(iteration, maxGen))
        case Op.InitPopulation(seed, size, shuffle) =>
          F.delay(initialiser.initialisePopulation(seed, size, shuffle))
        case Op.SelectFittest(population) =>
          F.delay(population.minBy(_._2))
        case Op.Cross(ind1, ind2, prob) =>
          F.delay(crossover.cross(ind1, ind2, prob))
        case Op.Mutate(ind, prob) =>
          F.delay(mutator.mutate(ind, prob))
        case Op.EvaluateOne(ind) =>
          F.delay(evaluator.evaluateIndividual(ind))
        case Op.EvaluatePopulation(population) =>
          apply(Op.ApplyToAll(population, i => Op.EvaluateOne(i)))
        case Op.SelectElites(population, popSize, ratio) =>
          F.delay(elitism.select(population, popSize * ratio))
        case Op.SelectPairs(population, limit) =>
          F.delay(selector.selectPairs(population, limit))
        case Op.ApplyToAll(population, op) =>
          Stream.emits(population).mapAsync(Int.MaxValue)(i => apply(op(i))).compile.toVector
        case _ | null =>
          F.raiseError(new IllegalArgumentException("Unexpected Op type: null or something else"))
  }
