package currexx.algorithms

import cats.effect.{IO, IOApp, Ref}
import cats.syntax.all.*
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.{Distance, SpeciesOperators}
import currexx.algorithms.progress.{Progress, Tracker}

import scala.concurrent.duration.FiniteDuration
import scala.util.Random

/** A small mechanism benchmark, not a claim about trading performance or equal evaluation budgets. Run with: sbt 'algorithms/Test/runMain
  * currexx.algorithms.SCGABenchmark'
  */
object SCGABenchmark extends IOApp.Simple:
  override def run: IO[Unit] =
    for
      _ <- IO.println("Two unequal peaks at -2.5 (9.0) and +2.5 (10.0); nominal population=40, generations=60.")
      _ <- IO.println("Equal generation budgets; request/cache-miss counts expose different evaluation costs. No validation holdout.")
      _ <- IO.println(
        "algorithm,seed,final_population,best_fitness,peaks_retained,evaluation_requests,unique_objective_evaluations,elapsed_ms"
      )
      _ <- List(7L, 19L, 43L).traverse_ { seed =>
        List(false, true).traverse_ { scga =>
          MultimodalExperiment.run(seed, scga).flatMap { result =>
            IO.println(
              f"${if (scga) "SCGA" else "GA"},$seed,${result.finalPopulation.size},${result.best}%.8f," +
                s"${result.peaksRetained},${result.requests},${result.objectiveCalls},${result.elapsed.toMillis}"
            )
          }
        }
      }
    yield ()

private[algorithms] object MultimodalExperiment:
  val Peaks: Vector[Double]  = Vector(-2.5, 2.5)
  private val PopulationSize = 40

  final case class Result(
      initial: Population[Double],
      finalPopulation: ValidatedPopulation[Double],
      generations: Vector[EvaluatedPopulation[Double]],
      requests: Int,
      objectiveCalls: Int,
      elapsed: FiniteDuration
  ):
    def best: Double       = finalPopulation.map(_._2.value).max
    def peaksRetained: Int = Peaks.count(peak => finalPopulation.exists(candidate => math.abs(candidate._1 - peak) <= 0.15))

  def fitness(x: Double): Fitness =
    Fitness(math.max(9.0 - math.pow(x + 2.5, 2), 10.0 - math.pow(x - 2.5, 2)))

  private val distance = new Distance[Double]:
    override def between(a: Double, b: Double): Either[IllegalArgumentException, Double] = Right(math.abs(a - b) / 10.0)

  private val crossover = new Crossover[IO, Double]:
    override def cross(a: Double, b: Double)(using r: Random): IO[Double] = IO.delay {
      val weight = r.nextDouble()
      weight * a + (1.0 - weight) * b
    }
    override def cross(a: Double, b: Double, probability: Double)(using r: Random): IO[Double] =
      IO.delay(r.nextDouble() < probability).flatMap(perform => if (perform) cross(a, b) else IO.pure(a))

  private val mutator = new Mutator[IO, Double]:
    override def mutate(individual: Double, probability: Double)(using r: Random): IO[Double] = IO.delay {
      if (r.nextDouble() >= probability) individual
      else
        val step = math.max(-0.4, math.min(0.4, r.nextGaussian() * 0.2))
        math.max(-5.0, math.min(5.0, individual + step))
    }

  def run(seed: Long, scga: Boolean, maxGen: Int = 60): IO[Result] = IO.defer {
    given Random   = new Random(seed)
    val gaParams   = Parameters.GA(PopulationSize, maxGen, 0.8, 0.6, 0.1, shuffle = true)
    val scgaParams = Parameters.SCGA.from(gaParams).copy(speciesRadius = 0.2, maxSpecies = 2, interspeciesMatingProbability = 0.1)
    for
      requests       <- Ref.of[IO, Int](0)
      objectiveCalls <- Ref.of[IO, Int](0)
      initial        <- Ref.of[IO, Population[Double]](Vector.empty)
      history        <- Ref.of[IO, Vector[EvaluatedPopulation[Double]]](Vector.empty)
      initialiser    <- Initialiser.custom[IO, Double] { (_, size, _) =>
        IO.delay {
          Vector.tabulate(size) { index =>
            val offset = 1.3 + summon[Random].nextDouble() * 0.4
            if (index % 2 == 0) Peaks.head - offset else Peaks.last + offset
          }
        }.flatTap(initial.set)
      }
      // This objective has no phase-dependent inputs, so candidate-only memoization is correct across Search and Rescore.
      cached <- Evaluator.cached[IO, Double](x => objectiveCalls.update(_ + 1) *> IO.delay(x -> fitness(x)))
      evaluator = new Evaluator[IO, Double]:
        override def evaluateIndividual(individual: Double, phase: EvaluationPhase): IO[(Double, Fitness)] =
          requests.update(_ + 1) *> cached.evaluateIndividual(individual, phase)
      tracker = new Tracker[IO, Double]:
        override def displayInitial(target: Double, params: Parameters[?]): IO[Unit] = IO.unit
        override def displayProgress(progress: Progress[Double]): IO[Unit]           =
          history.update(_ :+ progress.population)
        override def displayFinal(population: ValidatedPopulation[Double]): IO[Unit] = IO.unit
        override def displayNote(title: String, lines: List[String]): IO[Unit]       = IO.unit
      // Retain the complete final population so this experiment measures conserved basins rather than shortlist policy.
      validator       <- Validator.none[IO, Double]
      selector        <- Selector.tournament[IO, Double]
      elitism         <- Elitism.simple[IO, Double]
      species         <- SpeciesOperators.make[IO, Double](distance)
      start           <- IO.monotonic
      finalPopulation <-
        if (scga)
          Algorithm.SCGA
            .optimise(0.0, scgaParams)
            .foldMap(
              Op.scgaInterpreter(initialiser, crossover, mutator, evaluator, validator, species, tracker)
            )
        else
          Algorithm.GA
            .optimise(0.0, gaParams)
            .foldMap(
              Op.ioInterpreter(initialiser, crossover, mutator, evaluator, validator, selector, elitism, tracker)
            )
      end               <- IO.monotonic
      initialPopulation <- initial.get
      generations       <- history.get
      requestCount      <- requests.get
      objectiveCount    <- objectiveCalls.get
    yield Result(initialPopulation, finalPopulation, generations, requestCount, objectiveCount, end - start)
  }
