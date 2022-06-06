package currexx.algorithms.operators

import currexx.algorithms.{Fitness, Ind}

trait Evaluator[A]:
  def evaluateIndividual(individual: Ind[A]): (Ind[A], Fitness)
