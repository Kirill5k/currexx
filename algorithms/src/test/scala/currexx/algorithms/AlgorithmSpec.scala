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

      result.mkString mustBe
        """Initialise population of size 5 with shuffle=true
          |Iteration 1 of 2
          |Evaluate entire population
          |Select 1.25 elites from the current population
          |Distribute population in pairs
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Mutate individual with probability 0.2
          |Iteration 2 of 2
          |Evaluate entire population
          |Select 1.25 elites from the current population
          |Distribute population in pairs
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Crossover 2 individuals with probability 0.5
          |Applied to the entire population: Mutate individual with probability 0.2
          |Evaluate entire population
          |Select the fittest individual from the population
          |""".stripMargin
    }
  }

  def stateInterpreter[G]: Op[*, G] ~> State[List[String], *] = new (Op[*, G] ~> State[List[String], *]) {
    def apply[A](fa: Op[A, G]): State[List[String], A] = fa match {
      case Op.InitPopulation(seed, size, shuffle) =>
        State.modify[List[String]](_ :+ s"Initialise population of size $size with shuffle=$shuffle\n") >>
          State.pure(Vector.fill(size)(seed))
      case Op.UpdateOnProgress(i, maxGen) =>
        State.modify[List[String]](_ :+ s"Iteration $i of $maxGen\n")
      case Op.Cross(ind1, ind2, prob) =>
        State.modify[List[String]](_ :+ s"Crossover 2 individuals with probability $prob\n") >>
          State.pure(Some(ind1))
      case Op.Mutate(ind, prob) =>
        State.modify[List[String]](_ :+ s"Mutate individual with probability $prob\n") >>
          State.pure(ind)
      case Op.EvaluateOne(ind) =>
        State.modify[List[String]](_ :+ "Evaluate single individual\n") >>
          State.pure((ind, Fitness(1)))
      case Op.EvaluatePopulation(population) =>
        State.modify[List[String]](_ :+ "Evaluate entire population\n") >>
          State.pure(population.map(i => (i, Fitness(1))))
      case Op.SelectElites(population, popSize, ratio) =>
        State.modify[List[String]](_ :+ s"Select ${popSize * ratio} elites from the current population\n") >>
          State.pure(population.map((i, _) => i))
      case Op.SelectPairs(population, limit) =>
        State.modify[List[String]](_ :+ "Distribute population in pairs\n") >>
          State.pure(population.map((i, _) => (i, i)))
      case Op.SelectFittest(population) =>
        State.modify[List[String]](_ :+ "Select the fittest individual from the population\n") >>
          State.pure(population.head)
      case Op.ApplyToAll(population, op) =>
        State.modify[List[String]](_ :+ "Applied to the entire population: ") >>
          apply(op(population.head)).map(r => Vector(r))
      case _ | null => ???
    }
  }
}
