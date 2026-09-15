package currexx.algorithms

import cats.free.Free
import currexx.algorithms.operators.species.SpeciesStats
import currexx.algorithms.progress.Progress

sealed trait Alg
object Alg:
  sealed abstract class GA   extends Alg
  sealed abstract class SCGA extends Alg

sealed trait Parameters[A <: Alg]:
  def name: String
  def displayName: String
object Parameters {
  final case class GA(
      populationSize: Int,
      maxGen: Int,
      crossoverProbability: Double,
      mutationProbability: Double,
      elitismRatio: Double,
      shuffle: Boolean,
      initialOversampling: Int = 1
  ) extends Parameters[Alg.GA]:
    val name: String        = "GA"
    val displayName: String = "Genetic Algorithm"

  /** Species settings are initial defaults, not values fitted to an optimisation corpus. Champions occupy population slots; there is no
    * additional global elite population. Small populations automatically reduce the species cap so every species can breed.
    */
  final case class SCGA(
      populationSize: Int,
      maxGen: Int,
      crossoverProbability: Double,
      mutationProbability: Double,
      shuffle: Boolean,
      initialOversampling: Int = 1,
      speciesRadius: Double = 0.15,
      maxSpecies: Int = 8,
      interspeciesMatingProbability: Double = 0.10
  ) extends Parameters[Alg.SCGA]:
    val name: String             = "SCGA"
    val displayName: String      = "Species-Conserving Genetic Algorithm (SCGA)"
    def effectiveMaxSpecies: Int = math.min(maxSpecies, math.max(1, populationSize / 2))

  object SCGA:
    /** Copy the search budget and variation settings; conserved species replace GA's elitism ratio. */
    def fromGA(params: GA): SCGA = SCGA(
      populationSize = params.populationSize,
      maxGen = params.maxGen,
      crossoverProbability = params.crossoverProbability,
      mutationProbability = params.mutationProbability,
      shuffle = params.shuffle,
      initialOversampling = params.initialOversampling
    )
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
            _         <- Op.DisplayProgress(Progress.Population(i, params.maxGen, sortedPop)).freeM
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

  /** A bounded species-conserving GA with protected representatives and species-aware reproduction.
    *
    * Species are rebuilt each generation; conservation protects each current representative for one transition, not a permanent species
    * identity. Selection allocates one champion and at least one child per species before distributing the remaining budget by rank. The
    * injected validator controls the final shortlist; use Validator.speciesShortlisted to retain diversity at validation too.
    */
  case object SCGA extends Algorithm[Alg.SCGA, Parameters.SCGA] {
    override def optimise[I](target: I, params: Parameters.SCGA): Free[Op[*, I], ValidatedPopulation[I]] =
      for
        _              <- Op.DisplayInitial(target, params).freeM
        initial        <- Op.InitPopulation(target, params.populationSize * params.initialOversampling, params.shuffle).freeM
        initialEval    <- Op.EvaluatePopulation(initial, EvaluationPhase.Search(0)).freeM
        initialSpecies <- Op.IdentifySpecies(initialEval, params.speciesRadius, params.effectiveMaxSpecies).freeM
        conserved      <- Op.ConserveSpecies(initialSpecies, params.populationSize).freeM
        finalSpecies   <- iterate(conserved, params.maxGen) { (current, generation) =>
          for
            breeding  <- Op.SelectSpeciesPairs(current, params.populationSize, params.interspeciesMatingProbability).freeM
            crossed   <- Op.ApplyToAll(breeding.pairs, (pair: (I, I)) => Op.Cross(pair._1, pair._2, params.crossoverProbability)).freeM
            mutated   <- Op.ApplyToAll(crossed, (individual: I) => Op.Mutate(individual, params.mutationProbability)).freeM
            evaluated <- Op.EvaluatePopulation(mutated ++ current.representatives, EvaluationPhase.Search(generation)).freeM
            next      <- Op.SortByFitness(evaluated).freeM
            speciesStats = SpeciesStats(current.sizes, breeding.offspringCounts, current.distinctCandidates)
            _             <- Op.DisplayProgress(Progress.Species(generation, params.maxGen, next, speciesStats)).freeM
            nextSpecies   <- Op.IdentifySpecies(next, params.speciesRadius, params.effectiveMaxSpecies).freeM
            nextConserved <- Op.ConserveSpecies(nextSpecies, params.populationSize).freeM
          yield nextConserved
        }
        rescored  <- Op.EvaluatePopulation(finalSpecies.population, EvaluationPhase.Rescore).freeM
        ranked    <- Op.SortByFitness(rescored).freeM
        validated <- Op.ValidatePopulation(ranked).freeM
        _         <- Op.DisplayFinal(validated).freeM
      yield validated
  }

  private def iterate[F[_], A](a: A, n: Int)(f: (A, Int) => Free[F, A]): Free[F, A] =
    LazyList.range(1, n + 1).foldLeft[Free[F, A]](Free.pure(a))((res, i) => res.flatMap(r => f(r, i)))

}
