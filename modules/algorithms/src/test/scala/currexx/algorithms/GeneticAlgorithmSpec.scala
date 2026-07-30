package currexx.algorithms

import cats.effect.IO
import currexx.algorithms.operators.{Crossover, Elitism, Evaluator, Initialiser, Mutator, Selector, Validator}
import currexx.algorithms.progress.Tracker
import kirill5k.common.cats.test.IOWordSpec

import scala.util.Random

/** End-to-end check that the assembled GA actually optimises, rather than merely calling its operators in the right order.
  *
  * The target is OneMax: individuals are bit arrays and fitness is the number of ones, so the optimum is known and progress is unambiguous.
  * Operator-level specs cannot catch a loss of selection pressure, because each operator behaves correctly in isolation; without a test at
  * this level a GA that never converges still looks healthy.
  *
  * Elitism is deliberately switched off. Carrying the best individuals over unchanged drives OneMax towards the optimum on its own, which
  * would mask a selector that ranks parents but never amplifies them, so selection has to be the only thing that can make progress here.
  */
class GeneticAlgorithmSpec extends IOWordSpec {

  private val genomeLength   = 40
  private val populationSize = 40

  private def onesIn(individual: Array[Int]): Int = individual.count(_ == 1)

  private def meanOnes(population: ValidatedPopulation[Array[Int]]): Double =
    population.map(p => onesIn(p._1).toDouble).sum / population.size

  private def optimise(maxGen: Int)(using rand: Random): IO[ValidatedPopulation[Array[Int]]] =
    for
      initialiser <- Initialiser.simple[IO, Array[Int]](seed => IO(seed.map(_ => rand.nextInt(2))))
      crossover   <- Crossover.threeWaySplit[IO, Int]
      mutator     <- Mutator.bitFlip[IO]
      evaluator   <- Evaluator.cached[IO, Array[Int]](ind => IO.pure(ind -> Fitness(onesIn(ind).toDouble)))
      validator   <- Validator.none[IO, Array[Int]]
      selector    <- Selector.tournament[IO, Array[Int]]
      elitism     <- Elitism.simple[IO, Array[Int]]
      tracker     <- Tracker.noop[IO, Array[Int]]
      params = Parameters.GA(
        populationSize = populationSize,
        maxGen = maxGen,
        crossoverProbability = 0.7,
        mutationProbability = 0.02,
        elitismRatio = 0.0,
        shuffle = true
      )
      result <- Algorithm.GA
        .optimise[Array[Int]](Array.fill(genomeLength)(0), params)
        .foldMap(Op.ioInterpreter[IO, Array[Int]](initialiser, crossover, mutator, evaluator, validator, selector, elitism, tracker))
    yield result

  "Algorithm.GA" should {

    "converge towards the optimum over successive generations" in {
      given Random = Random(42)

      val result = for
        early <- optimise(1)
        late  <- optimise(60)
      yield (onesIn(early.head._1), onesIn(late.head._1))

      result.asserting { case (bestEarly, bestLate) =>
        bestLate must be > bestEarly
        bestLate must be > (genomeLength * 0.9).toInt
      }
    }

    "raise the fitness of the whole population, not just the elites" in {
      given Random = Random(7)

      val result = for
        early <- optimise(1)
        late  <- optimise(60)
      yield (meanOnes(early), meanOnes(late))

      result.asserting { case (meanEarly, meanLate) =>
        // Elitism on its own would carry a handful of good individuals while leaving the rest at random-chance
        // fitness. Only genuine selection pressure lifts the population average.
        meanLate must be > (meanEarly * 1.3)
      }
    }
  }
}
