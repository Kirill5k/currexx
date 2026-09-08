package currexx.algorithms

import cats.free.Free

sealed trait Alg
object Alg:
  sealed abstract class GA extends Alg

sealed trait Parameters[A <: Alg]
object Parameters {
  final case class GA(
      populationSize: Int,
      maxGen: Int,
      crossoverProbability: Double,
      mutationProbability: Double,
      elitismRatio: Double,
      shuffle: Boolean,
      initialOversampling: Int = 1
  ) extends Parameters[Alg.GA]
}

sealed trait Algorithm[A <: Alg, P <: Parameters[A]]:
  def optimise[I](target: I, params: P): Free[Op[*, I], ValidatedPopulation[I]]

object Algorithm {
  case object GA extends Algorithm[Alg.GA, Parameters.GA] {
    override def optimise[I](target: I, params: Parameters.GA): Free[Op[*, I], ValidatedPopulation[I]] =
      for
        _ <- Op.DisplayInitial(target, params).freeM
        // Truncation happens after the sort, which is what makes over-drawing a selection rather than a bigger population. Whether it buys
        // anything depends on the members differing from each other, which is the initialiser's business and not knowable here - a caller
        // that wants exactly what it will keep asks for an oversampling of one.
        initialSize = params.populationSize * params.initialOversampling
        pop         <- Op.InitPopulation(target, initialSize, params.shuffle).freeM
        initialEval <- Op.EvaluatePopulation(pop, EvaluationPhase.Search(0)).freeM
        initialSort <- Op.SortByFitness(initialEval).freeM
        finalPop    <- iterate(initialSort.take(params.populationSize), params.maxGen) { (currentPop, i) =>
          for
            elites    <- Op.SelectElites(currentPop, params.populationSize, params.elitismRatio).freeM
            pairs     <- Op.SelectPairs(currentPop, params.populationSize).freeM
            crossed1  <- Op.ApplyToAll(pairs, (pair: (I, I)) => Op.Cross(pair._1, pair._2, params.crossoverProbability)).freeM
            crossed2  <- Op.ApplyToAll(pairs, (pair: (I, I)) => Op.Cross(pair._2, pair._1, params.crossoverProbability)).freeM
            mutated   <- Op.ApplyToAll(crossed1 ++ crossed2, (ind: I) => Op.Mutate(ind, params.mutationProbability)).freeM
            evPop     <- Op.EvaluatePopulation(mutated ++ elites, EvaluationPhase.Search(i)).freeM
            sortedPop <- Op.SortByFitness(evPop).freeM
            _         <- Op.DisplayProgress(i, params.maxGen, sortedPop).freeM
          yield sortedPop
        }
        // Scored once more on everything the objective has, because the ranking selection finished with was made under whatever reading
        // that generation asked for, and a figure a run reports has to mean what another run's does. Re-sorting matters as much as
        // re-scoring: `Op.ValidatePopulation` shortlists from the front, so a population rescored and left in its old order would spend
        // the held-out evidence on candidates chosen by the numbers that were just replaced.
        rescored <- Op.EvaluatePopulation(finalPop.map(_._1), EvaluationPhase.Rescore).freeM
        ranked   <- Op.SortByFitness(rescored).freeM
        // Validation is the last thing the run does and it happens exactly once, on the population selection has
        // finished with. Anything earlier would be the search reading the held-out sample, which is the one thing that
        // stops it being held out. Returning the validated population rather than the evaluated one is what makes a
        // champion impossible to obtain without the reading that says whether it means anything.
        validatedPop <- Op.ValidatePopulation(ranked).freeM
        _            <- Op.DisplayFinal(validatedPop).freeM
      yield validatedPop
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
