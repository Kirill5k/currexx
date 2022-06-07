package currexx.algorithms

import cats.~>
import cats.effect.{Async, Sync}
import cats.free.Free
import currexx.algorithms.operators.*
import currexx.algorithms.collections.*
import fs2.Stream

import scala.reflect.ClassTag
import scala.util.Random

opaque type Fitness <: BigDecimal = BigDecimal
object Fitness:
  def apply(value: BigDecimal): Fitness              = value
  extension (fitness: Fitness) def value: BigDecimal = fitness
  given ordering: Ordering[Fitness] with
    def compare(f1: Fitness, f2: Fitness) = f1.compare(f2)

type Ind[G]                   = Array[G]
type Population[G]            = List[Ind[G]]
type EvaluatedPopulation[G]   = List[(Ind[G], Fitness)]
type DistributedPopulation[G] = List[(Ind[G], Ind[G])]

enum Op[A, G]:
  case UpdateOnProgress[G](iteration: Int, maxGen: Int)                                 extends Op[Unit, G]
  case InitPopulation[G](seed: Ind[G], size: Int, shuffle: Boolean)                     extends Op[Population[G], G]
  case Cross[G](ind1: Ind[G], ind2: Ind[G], prob: Double)                               extends Op[Option[Ind[G]], G]
  case Mutate[G](ind: Ind[G], prob: Double)                                             extends Op[Ind[G], G]
  case EvaluateOne[G](ind: Ind[G])                                                      extends Op[(Ind[G], Fitness), G]
  case EvaluatePopulation[G](population: Population[G])                                 extends Op[EvaluatedPopulation[G], G]
  case SelectElites[G](population: EvaluatedPopulation[G], popSize: Int, ratio: Double) extends Op[Population[G], G]
  case SelectPairs[G](population: EvaluatedPopulation[G], limit: Int)                   extends Op[DistributedPopulation[G], G]
  case SelectFittest[G](population: EvaluatedPopulation[G])                             extends Op[(Ind[G], Fitness), G]
  case ApplyToAll[A, B, G](population: List[A], op: A => Op[B, G])                      extends Op[List[B], G]

object Op:
  extension [A, G](fa: Op[A, G]) def freeM: Free[Op[*, G], A] = Free.liftF(fa)

  inline def ioInterpreter[F[_], G: ClassTag](
      crossover: Crossover[G],
      mutator: Mutator[G],
      evaluator: Evaluator[G],
      selector: Selector[G],
      elitism: Elitism[G],
      updateFn: Option[(Int, Int) => F[Unit]] = None
  )(using F: Async[F], rand: Random): Op[*, G] ~> F = new (Op[*, G] ~> F) {
    def apply[A](fa: Op[A, G]): F[A] =
      fa match
        case Op.UpdateOnProgress(iteration, maxGen) =>
          updateFn.fold(F.unit)(f => f(iteration, maxGen))
        case Op.InitPopulation(seed, size, shuffle) =>
          F.delay(List.fill(size)(if (shuffle) seed.shuffle else seed))
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
          F.delay(selector.selectPairs(population, limit).toList)
        case Op.ApplyToAll(population, op) =>
          Stream.emits(population).mapAsync(Int.MaxValue)(i => apply(op(i))).compile.toList
        case _ | null =>
          F.raiseError(new IllegalArgumentException("Unexpected Op type: null or something else"))
  }
