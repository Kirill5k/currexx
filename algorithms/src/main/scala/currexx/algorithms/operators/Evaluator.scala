package currexx.algorithms.operators

import currexx.algorithms.Fitness

trait Evaluator[F[_], I]:
  def evaluateIndividual(individual: I): F[(I, Fitness)]
