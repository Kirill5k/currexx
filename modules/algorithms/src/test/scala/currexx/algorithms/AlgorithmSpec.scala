package currexx.algorithms

import cats.data.State
import cats.~>
import cats.syntax.flatMap.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AlgorithmSpec extends AnyWordSpec with Matchers {

  "Algorithm.GA (Genetic Algorithm)" should {
    val ind    = Array.range(0, 10)
    val params = Parameters.GA(5, 2, 0.5, 0.2, 0.25, true)

    "optimize a target by applying principles of natural selection" in {
      val optResult = Algorithm.GA.optimise(ind, params)
      val result    = optResult.foldMap(stateInterpreter).run(List.empty).value._1

      // The phases are part of what is being asserted, not decoration. An objective that reads a different slice of its evidence per
      // generation is relying on these numbers running 0, 1, 2 with no gaps and on `Rescore` arriving exactly once, after the last of
      // them and before validation - and inferring any of that by counting evaluations is the coupling the phase exists to remove.
      result.mkString mustBe
        """Starting GA
          |Initialise population of size 5 with shuffle=true
          |Evaluate entire population for Search(0)
          |Sorting evaluated population by fitness
          |Select 1.25 elites from the current population
          |Distribute population in pairs
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Mutate individual with probability 0.2
          |Evaluate entire population for Search(1)
          |Sorting evaluated population by fitness
          |Iteration 1 of 2
          |Select 1.25 elites from the current population
          |Distribute population in pairs
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Mutate individual with probability 0.2
          |Evaluate entire population for Search(2)
          |Sorting evaluated population by fitness
          |Iteration 2 of 2
          |Evaluate entire population for Rescore
          |Sorting evaluated population by fitness
          |Validate the finished population
          |Final population displayed
          |""".stripMargin
    }
  }

  def stateInterpreter[G]: Op[*, G] ~> State[List[String], *] = new ~>[Op[*, G], State[List[String], *]] {
    def apply[A](fa: Op[A, G]): State[List[String], A] = fa match {
      case Op.DisplayInitial(_, _) =>
        State.modify[List[String]](_ :+ "Starting GA\n")
      case Op.InitPopulation(seed, size, shuffle) =>
        State.modify[List[String]](_ :+ s"Initialise population of size $size with shuffle=$shuffle\n") >>
          State.pure(Vector.fill(size)(seed))
      case Op.DisplayProgress(i, maxGen, _) =>
        State.modify[List[String]](_ :+ s"Iteration $i of $maxGen\n")
      case Op.DisplayFinal(_) =>
        State.modify[List[String]](_ :+ "Final population displayed\n")
      case Op.Cross(ind1, ind2, prob) =>
        State.modify[List[String]](_ :+ s"Crossover 2 individuals with probability $prob\n") >>
          State.pure(ind1)
      case Op.Mutate(ind, prob) =>
        State.modify[List[String]](_ :+ s"Mutate individual with probability $prob\n") >>
          State.pure(ind)
      case Op.EvaluatePopulation(population, phase) =>
        State.modify[List[String]](_ :+ s"Evaluate entire population for $phase\n") >>
          State.pure(population.map(i => (i, Fitness(1.0))))
      case Op.SelectElites(population, popSize, ratio) =>
        State.modify[List[String]](_ :+ s"Select ${popSize * ratio} elites from the current population\n") >>
          State.pure(population.map((i, _) => i))
      case Op.SelectPairs(population, limit) =>
        State.modify[List[String]](_ :+ "Distribute population in pairs\n") >>
          State.pure(population.map((i, _) => (i, i)))
      case Op.ValidatePopulation(population) =>
        State.modify[List[String]](_ :+ "Validate the finished population\n") >>
          State.pure(population.map((i, f) => (i, f, f)))
      case Op.SortByFitness(population) =>
        State.modify[List[String]](_ :+ "Sorting evaluated population by fitness\n") >>
          State.pure(population.sortBy(_._2)(using Ordering[Fitness].reverse))
      case Op.ApplyToAll(population, op) =>
        State.modify[List[String]](_ :+ "Applied to the entire population: ") >>
          apply(op(population.head)).map(r => Vector(r))
      case _ | null => ???
    }
  }
}
