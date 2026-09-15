package currexx.algorithms.progress

import cats.effect.{IO, Ref}
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.operators.species.SpeciesStats
import currexx.algorithms.{Fitness, Parameters, ValidatedPopulation}

class CompositeTrackerSpec extends IOWordSpec {

  "CompositeTracker" should {
    val params = Parameters.GA(100, 10, 0.5, 0.1, 0.1, true)

    "execute all trackers" in {
      val result = for {
        ref1 <- Ref.of[IO, Int](0)
        ref2 <- Ref.of[IO, Int](0)
        tracker1 = new Tracker[IO, String] {
          override def displayInitial(target: String, params: Parameters[?]): IO[Unit] = ref1.update(_ + 1)
          override def displayProgress(progress: Progress[String]): IO[Unit]           = ref1.update(_ + 1)
          override def displayFinal(population: ValidatedPopulation[String]): IO[Unit] = ref1.update(_ + 1)
          override def displayNote(title: String, lines: List[String]): IO[Unit]       = ref1.update(_ + 1)
        }
        tracker2 = new Tracker[IO, String] {
          override def displayInitial(target: String, params: Parameters[?]): IO[Unit] = ref2.update(_ + 1)
          override def displayProgress(progress: Progress[String]): IO[Unit]           = ref2.update(_ + 1)
          override def displayFinal(population: ValidatedPopulation[String]): IO[Unit] = ref2.update(_ + 1)
          override def displayNote(title: String, lines: List[String]): IO[Unit]       = ref2.update(_ + 1)
        }
        composite = CompositeTracker.make[IO, String](tracker1, tracker2)
        _      <- composite.displayInitial("target", params)
        _      <- composite.displayProgress(Progress.Population(1, 10, Vector.empty))
        _      <- composite.displayProgress(Progress.Species(1, 10, Vector.empty, SpeciesStats(Vector(4, 2), Vector(3, 1), 5)))
        _      <- composite.displayFinal(Vector.empty)
        _      <- composite.displayNote("note", List("line"))
        count1 <- ref1.get
        count2 <- ref2.get
      } yield (count1, count2)

      result.asserting { case (count1, count2) =>
        count1 mustBe 5
        count2 mustBe 5
      }
    }

    "forward parameters and both progress variants unchanged" in {
      val scgaParams         = Parameters.SCGA(10, 20, 0.5, 0.1, shuffle = true)
      val stats              = SpeciesStats(Vector(6, 4), Vector(5, 3), 8)
      val population         = Vector("completed-candidate" -> Fitness(7.0))
      val populationProgress = Progress.Population(9, 20, population)
      val speciesProgress    = Progress.Species(10, 20, population, stats)
      val result             = for {
        initial   <- Ref.of[IO, Vector[(String, Parameters[?])]](Vector.empty)
        telemetry <- Ref.of[IO, Vector[Progress[String]]](Vector.empty)
        tracker = new Tracker[IO, String] {
          override def displayInitial(target: String, params: Parameters[?]): IO[Unit] = initial.update(_ :+ (target, params))
          override def displayProgress(progress: Progress[String]): IO[Unit]           = telemetry.update(_ :+ progress)
          override def displayFinal(population: ValidatedPopulation[String]): IO[Unit] = IO.unit
          override def displayNote(title: String, lines: List[String]): IO[Unit]       = IO.unit
        }
        composite = CompositeTracker.make[IO, String](tracker, tracker)
        _       <- composite.displayInitial("scga-target", scgaParams)
        _       <- composite.displayProgress(populationProgress)
        _       <- composite.displayProgress(speciesProgress)
        inits   <- initial.get
        samples <- telemetry.get
      } yield (inits, samples)

      result.asserting { case (inits, samples) =>
        inits mustBe Vector.fill(2)(("scga-target", scgaParams))
        samples mustBe Vector(populationProgress, populationProgress, speciesProgress, speciesProgress)
      }
    }
  }
}
