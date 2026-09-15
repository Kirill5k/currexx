package currexx.algorithms.operators

import cats.effect.Concurrent
import cats.syntax.functor.*
import currexx.algorithms.{EvaluationPhase, Fitness, memoize}

trait Evaluator[F[_], I]:
  /** Scores one individual for the supplied phase. Fitness may differ between phases; a previously evaluated individual does not imply that
    * its score remains valid in another phase. Implementations with a fixed objective may ignore the phase.
    */
  def evaluateIndividual(individual: I, phase: EvaluationPhase): F[(I, Fitness)]

object Evaluator:

  /** Scores each distinct individual once, however many times the search asks for it.
    *
    * Sound because `objectiveFn` takes the individual and nothing else: an objective that cannot see the phase cannot vary with it, so the
    * fitness computed under one phase is the answer under all of them. That is a property of this signature, not a promise made in prose -
    * an objective that does need the phase cannot be passed here at all.
    *
    * An evaluator whose fitness depends on the phase can instead cache intermediate results that remain valid across phases.
    */
  def cached[F[_]: Concurrent, I](objectiveFn: I => F[(I, Fitness)]): F[Evaluator[F, I]] =
    memoize[F, I, Fitness](individual => objectiveFn(individual).map(_._2)).map { fitnessOf =>
      new Evaluator[F, I]:
        override def evaluateIndividual(individual: I, phase: EvaluationPhase): F[(I, Fitness)] =
          fitnessOf(individual).map(individual -> _)
    }
