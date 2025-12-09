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
        pop         <- Op.InitPopulation(target, params.populationSize, params.shuffle).freeM
        initialEval <- Op.EvaluatePopulation(pop).freeM
        initialSort <- Op.SortByFitness(initialEval).freeM
        finalPop    <- iterate(initialSort, params.maxGen) { (currentPop, i) =>
          for
            elites      <- Op.SelectElites(currentPop, params.populationSize, params.elitismRatio).freeM
            pairs       <- Op.SelectPairs(currentPop, params.populationSize).freeM
            crossed1    <- Op.ApplyToAll(pairs, pair => Op.Cross(pair._1, pair._2, params.crossoverProbability)).freeM
            crossed2    <- Op.ApplyToAll(pairs, pair => Op.Cross(pair._2, pair._1, params.crossoverProbability)).freeM
            mutated     <- Op.ApplyToAll(crossed1 ++ crossed2, ind => Op.Mutate(ind, params.mutationProbability)).freeM
            evPop       <- Op.EvaluatePopulation(mutated ++ elites).freeM
            sortedPop   <- Op.SortByFitness(evPop).freeM
            _           <- Op.UpdateOnProgress(i, params.maxGen, sortedPop).freeM
          yield sortedPop
        }
      yield finalPop
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
