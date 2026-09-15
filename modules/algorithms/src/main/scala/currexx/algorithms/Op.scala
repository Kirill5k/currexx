package currexx.algorithms

import cats.syntax.traverse.*
import cats.syntax.flatMap.*
import cats.~>
import cats.effect.Async
import cats.free.Free
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.*
import currexx.algorithms.progress.{Progress, Tracker}
import fs2.Stream

import scala.util.{Random, Try}

opaque type Fitness = Double
object Fitness:
  def apply(value: Double): Fitness = value
  extension (fitness: Fitness)
    def isZero: Boolean            = fitness == 0.0
    def -(other: Fitness): Fitness = fitness - other
    def +(other: Fitness): Fitness = fitness + other
    def /(other: Fitness): Fitness = fitness / other
    def >(other: Fitness): Boolean = fitness > other
    def <(other: Fitness): Boolean = fitness < other
    def value: Double              = fitness
  given ordering: Ordering[Fitness] with
    def compare(f1: Fitness, f2: Fitness): Int = java.lang.Double.compare(f1, f2)

  given numeric: Numeric[Fitness] with
    def plus(x: Fitness, y: Fitness): Fitness     = x + y
    def minus(x: Fitness, y: Fitness): Fitness    = x - y
    def times(x: Fitness, y: Fitness): Fitness    = x * y
    def negate(x: Fitness): Fitness               = -x
    def fromInt(x: Int): Fitness                  = x.toDouble
    def parseString(str: String): Option[Fitness] = Try(str.toDouble).toOption
    def toInt(x: Fitness): Int                    = x.toInt
    def toLong(x: Fitness): Long                  = x.toLong
    def toFloat(x: Fitness): Float                = x.toFloat
    def toDouble(x: Fitness): Double              = x
    def compare(x: Fitness, y: Fitness): Int      = java.lang.Double.compare(x, y)

type Population[I]            = Vector[I]
type EvaluatedPopulation[I]   = Vector[(I, Fitness)]
type DistributedPopulation[I] = Vector[(I, I)]

/** An individual with the fitness the search ranked it by and the fitness it went on to earn against evidence the search never saw, in that
  * order. The second is the one worth concluding from; both are kept because it is the pair that says whether a run found anything, and
  * either alone reads as a result.
  */
type ValidatedPopulation[I] = Vector[(I, Fitness, Fitness)]

enum Op[A, I]:
  case DisplayInitial[I](target: I, params: Parameters[?])                                     extends Op[Unit, I]
  case DisplayProgress[I](progress: Progress[I])                                               extends Op[Unit, I]
  case DisplayFinal[I](population: ValidatedPopulation[I])                                     extends Op[Unit, I]
  case InitPopulation[I](seed: I, size: Int, shuffle: Boolean)                                 extends Op[Population[I], I]
  case Cross[I](ind1: I, ind2: I, prob: Double)                                                extends Op[I, I]
  case Mutate[I](ind: I, prob: Double)                                                         extends Op[I, I]
  case EvaluatePopulation[I](population: Population[I], phase: EvaluationPhase)                extends Op[EvaluatedPopulation[I], I]
  case ValidatePopulation[I](population: EvaluatedPopulation[I])                               extends Op[ValidatedPopulation[I], I]
  case SelectElites[I](population: EvaluatedPopulation[I], popSize: Int, ratio: Double)        extends Op[Population[I], I]
  case SelectPairs[I](population: EvaluatedPopulation[I], limit: Int)                          extends Op[DistributedPopulation[I], I]
  case IdentifySpecies[I](population: EvaluatedPopulation[I], radius: Double, maxSpecies: Int) extends Op[SpeciesPopulation[I], I]
  case SelectSpeciesPairs[I](species: SpeciesPopulation[I], populationSize: Int, interspeciesProbability: Double)
      extends Op[SpeciesBreeding[I], I]
  case ConserveSpecies[I](species: SpeciesPopulation[I], size: Int)      extends Op[SpeciesPopulation[I], I]
  case SortByFitness[I](population: EvaluatedPopulation[I])              extends Op[EvaluatedPopulation[I], I]
  case ApplyToAll[A, B, I](population: Population[A], op: A => Op[B, I]) extends Op[Population[B], I]

object Op:
  extension [A, I](fa: Op[A, I]) def freeM: Free[Op[*, I], A] = Free.liftF(fa)

  sealed abstract class OpInterpreter[F[_], I](
      initialiser: Initialiser[F, I],
      crossover: Crossover[F, I],
      mutator: Mutator[F, I],
      evaluator: Evaluator[F, I],
      validator: Validator[F, I],
      progressTracker: Tracker[F, I]
  )(using F: Async[F], rand: Random)
      extends ~>[Op[*, I], F] {
    protected def applySpecific[A](op: Op[A, I]): F[A]

    final def apply[A](fa: Op[A, I]): F[A] =
      fa match
        case Op.DisplayInitial(target, params) =>
          progressTracker.displayInitial(target, params)
        case Op.DisplayProgress(progress) =>
          progressTracker.displayProgress(progress)
        case Op.DisplayFinal(population) =>
          progressTracker.displayFinal(population)
        case Op.InitPopulation(seed, size, shuffle) =>
          initialiser.initialisePopulation(seed, size, shuffle)
        case Op.Cross(ind1, ind2, prob) =>
          crossover.cross(ind1, ind2, prob)
        case Op.Mutate(ind, prob) =>
          mutator.mutate(ind, prob)
        case Op.EvaluatePopulation(population, phase) =>
          val parallelism = Math.max(1, Runtime.getRuntime.availableProcessors())
          Stream.emits(population).mapAsync(parallelism)(evaluator.evaluateIndividual(_, phase)).compile.toVector
        case Op.ValidatePopulation(population) =>
          validator.validate(population)
        case Op.SortByFitness(population) =>
          F.delay(population.sortBy(_._2)(using Ordering[Fitness].reverse))
        case Op.ApplyToAll(population, op) =>
          population.traverse(i => apply(op(i)))
        case op => applySpecific(op)
  }

  final class GAInterpreter[F[_], I](
      initialiser: Initialiser[F, I],
      crossover: Crossover[F, I],
      mutator: Mutator[F, I],
      evaluator: Evaluator[F, I],
      validator: Validator[F, I],
      selector: Selector[F, I],
      elitism: Elitism[F, I],
      progressTracker: Tracker[F, I]
  )(using F: Async[F], rand: Random)
      extends OpInterpreter(initialiser, crossover, mutator, evaluator, validator, progressTracker):
    override protected def applySpecific[A](op: Op[A, I]): F[A] =
      op match
        case Op.SelectElites(population, popSize, ratio) =>
          elitism.select(population, popSize * ratio)
        case Op.SelectPairs(population, limit) =>
          selector.selectPairs(population, limit)
        case other =>
          F.raiseError(new IllegalStateException(s"GA interpreter does not support ${other.productPrefix}"))

  final class SCGAInterpreter[F[_], I](
      initialiser: Initialiser[F, I],
      crossover: Crossover[F, I],
      mutator: Mutator[F, I],
      evaluator: Evaluator[F, I],
      validator: Validator[F, I],
      species: SpeciesOperators[F, I],
      progressTracker: Tracker[F, I]
  )(using F: Async[F], rand: Random)
      extends OpInterpreter(initialiser, crossover, mutator, evaluator, validator, progressTracker):
    override protected def applySpecific[A](op: Op[A, I]): F[A] =
      op match
        case Op.IdentifySpecies(population, radius, maxSpecies) =>
          species.speciation.partition(population, radius, maxSpecies).flatMap(F.fromEither)
        case Op.SelectSpeciesPairs(groups, populationSize, interspeciesProbability) =>
          species.selection.select(groups, populationSize, interspeciesProbability).flatMap(F.fromEither)
        case Op.ConserveSpecies(groups, size) =>
          species.conservation.conserve(groups, size).flatMap(F.fromEither)
        case other =>
          F.raiseError(new IllegalStateException(s"SCGA interpreter does not support ${other.productPrefix}"))

  inline def ioInterpreter[F[_], I](
      initialiser: Initialiser[F, I],
      crossover: Crossover[F, I],
      mutator: Mutator[F, I],
      evaluator: Evaluator[F, I],
      validator: Validator[F, I],
      selector: Selector[F, I],
      elitism: Elitism[F, I],
      progressTracker: Tracker[F, I]
  )(using F: Async[F], rand: Random): Op[*, I] ~> F =
    new GAInterpreter[F, I](initialiser, crossover, mutator, evaluator, validator, selector, elitism, progressTracker)

  def scgaInterpreter[F[_], I](
      initialiser: Initialiser[F, I],
      crossover: Crossover[F, I],
      mutator: Mutator[F, I],
      evaluator: Evaluator[F, I],
      validator: Validator[F, I],
      species: SpeciesOperators[F, I],
      progressTracker: Tracker[F, I]
  )(using F: Async[F], rand: Random): Op[*, I] ~> F =
    new SCGAInterpreter(initialiser, crossover, mutator, evaluator, validator, species, progressTracker)
