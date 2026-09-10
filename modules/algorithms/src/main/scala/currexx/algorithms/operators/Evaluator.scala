package currexx.algorithms.operators

import cats.effect.Concurrent
import cats.syntax.functor.*
import currexx.algorithms.{EvaluationPhase, Fitness, memoize}

trait Evaluator[F[_], I]:
  /** Scores one individual. The phase is offered rather than assumed: an objective that reads a different slice of its evidence as the
    * search proceeds needs to be told which reading is being asked for, and one that does not can ignore it.
    */
  def evaluateIndividual(individual: I, phase: EvaluationPhase): F[(I, Fitness)]

object Evaluator:

  /** Scores each distinct individual once, however many times the search asks for it.
    *
    * Sound because `objectiveFn` takes the individual and nothing else: an objective that cannot see the phase cannot vary with it, so the
    * fitness computed under one phase is the answer under all of them. That is a property of this signature, not a promise made in prose -
    * an objective that does need the phase cannot be passed here at all.
    *
    * Which is the whole trick for a phase-dependent objective: memoise the part of it that does not depend on the phase, rather than the
    * fitness. `IndicatorObjective` caches per-fold scores after backtesting and recombines them per phase for nothing.
    */
  def cached[F[_]: Concurrent, I](objectiveFn: I => F[(I, Fitness)]): F[Evaluator[F, I]] =
    memoize[F, I, Fitness](individual => objectiveFn(individual).map(_._2)).map { fitnessOf =>
      new Evaluator[F, I]:
        override def evaluateIndividual(individual: I, phase: EvaluationPhase): F[(I, Fitness)] =
          fitnessOf(individual).map(individual -> _)
    }
