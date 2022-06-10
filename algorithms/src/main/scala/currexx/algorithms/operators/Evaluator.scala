package currexx.algorithms.operators

import currexx.algorithms.{Fitness}

trait Evaluator[I]:
  def evaluateIndividual(individual: I): (I, Fitness)
