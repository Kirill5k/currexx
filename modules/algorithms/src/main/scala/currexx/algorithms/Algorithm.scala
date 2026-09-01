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
        // Over-drawing only means anything where the members differ from each other, so an unshuffled run asks for exactly what it will
        // keep. Truncation happens after the sort, which is what makes this a selection rather than a bigger population.
        initialSize = if (params.shuffle) params.populationSize * params.initialOversampling else params.populationSize
        pop         <- Op.InitPopulation(target, initialSize, params.shuffle).freeM
        initialEval <- Op.EvaluatePopulation(pop).freeM
        initialSort <- Op.SortByFitness(initialEval).freeM
        finalPop    <- iterate(initialSort.take(params.populationSize), params.maxGen) { (currentPop, i) =>
          for
            elites    <- Op.SelectElites(currentPop, params.populationSize, params.elitismRatio).freeM
            pairs     <- Op.SelectPairs(currentPop, params.populationSize).freeM
            crossed1  <- Op.ApplyToAll(pairs, (pair: (I, I)) => Op.Cross(pair._1, pair._2, params.crossoverProbability)).freeM
            crossed2  <- Op.ApplyToAll(pairs, (pair: (I, I)) => Op.Cross(pair._2, pair._1, params.crossoverProbability)).freeM
            mutated   <- Op.ApplyToAll(crossed1 ++ crossed2, (ind: I) => Op.Mutate(ind, params.mutationProbability)).freeM
            evPop     <- Op.EvaluatePopulation(mutated ++ elites).freeM
            sortedPop <- Op.SortByFitness(evPop).freeM
            _         <- Op.DisplayProgress(i, params.maxGen, sortedPop).freeM
          yield sortedPop
        }
        // Validation is the last thing the run does and it happens exactly once, on the population selection has
        // finished with. Anything earlier would be the search reading the held-out sample, which is the one thing that
        // stops it being held out. Returning the validated population rather than the evaluated one is what makes a
        // champion impossible to obtain without the reading that says whether it means anything.
        validatedPop <- Op.ValidatePopulation(finalPop).freeM
        _            <- Op.DisplayFinal(validatedPop).freeM
      yield validatedPop
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
