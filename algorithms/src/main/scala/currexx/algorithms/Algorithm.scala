package currexx.algorithms

import cats.free.Free

sealed trait Parameters
object Parameters {
  final case class GA(
      populationSize: Int,
      maxGen: Int,
      crossoverProbability: Double,
      mutationProbability: Double,
      elitismRatio: Double,
      shuffle: Boolean
  ) extends Parameters
}

sealed trait Algorithm[P <: Parameters]:
  def optimize[G](target: Ind[G], params: P): Free[Op[*, G], (Ind[G], Fitness)]

object Algorithm {
  case object GA extends Algorithm[Parameters.GA] {
    override def optimize[G](target: Ind[G], params: Parameters.GA): Free[Op[*, G], (Ind[G], Fitness)] =
      for
        pop <- Op.InitPopulation(target, params.populationSize, params.shuffle).freeM
        finalPop <- iterate(pop, params.maxGen) { (currentPop, i) =>
          for
            _        <- Op.UpdateOnProgress(i, params.maxGen).freeM
            evPop    <- Op.EvaluatePopulation(currentPop).freeM
            elites   <- Op.SelectElites(evPop, params.populationSize, params.elitismRatio).freeM
            pairs    <- Op.SelectPairs(evPop, params.populationSize).freeM
            crossed1 <- Op.ApplyToAll(pairs, (i1, i2) => Op.Cross(i1, i2, params.crossoverProbability)).freeM
            crossed2 <- Op.ApplyToAll(pairs, (i1, i2) => Op.Cross(i2, i1, params.crossoverProbability)).freeM
            mutated  <- Op.ApplyToAll(crossed1 ++ crossed2, i => Op.Mutate(i, params.mutationProbability)).freeM
          yield mutated ++ elites
        }
        evPop   <- Op.EvaluatePopulation(finalPop).freeM
        fittest <- Op.SelectFittest(evPop).freeM
      yield fittest
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
