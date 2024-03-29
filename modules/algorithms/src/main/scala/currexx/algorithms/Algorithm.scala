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
      shuffle: Boolean
  ) extends Parameters[Alg.GA]
}

sealed trait Algorithm[A <: Alg, P <: Parameters[A]]:
  def optimise[I](target: I, params: P): Free[Op[*, I], EvaluatedPopulation[I]]

object Algorithm {
  case object GA extends Algorithm[Alg.GA, Parameters.GA] {
    override def optimise[I](target: I, params: Parameters.GA): Free[Op[*, I], EvaluatedPopulation[I]] =
      for
        pop <- Op.InitPopulation(target, params.populationSize, params.shuffle).freeM
        finalPop <- iterate(pop, params.maxGen) { (currentPop, i) =>
          for
            _           <- Op.UpdateOnProgress(i, params.maxGen).freeM
            evPop       <- Op.EvaluatePopulation(currentPop).freeM
            sortedEvPop <- Op.SortByFitness(evPop).freeM
            elites      <- Op.SelectElites(sortedEvPop, params.populationSize, params.elitismRatio).freeM
            pairs       <- Op.SelectPairs(sortedEvPop, params.populationSize).freeM
            crossed1    <- Op.ApplyToAll(pairs, (i1, i2) => Op.Cross(i1, i2, params.crossoverProbability)).freeM
            crossed2    <- Op.ApplyToAll(pairs, (i1, i2) => Op.Cross(i2, i1, params.crossoverProbability)).freeM
            mutated     <- Op.ApplyToAll(crossed1 ++ crossed2, i => Op.Mutate(i, params.mutationProbability)).freeM
          yield mutated ++ elites
        }
        evPop    <- Op.EvaluatePopulation(finalPop).freeM
        finalPop <- Op.SortByFitness(evPop).freeM
      yield finalPop
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
