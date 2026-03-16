package currexx.algorithms.progress

import cats.effect.{IO, Ref}
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.{EvaluatedPopulation, Parameters}

class CompositeTrackerSpec extends IOWordSpec {

  "CompositeTracker" should {
    val params = Parameters.GA(100, 10, 0.5, 0.1, 0.1, true)

    "execute all trackers" in {
      val result = for {
        ref1 <- Ref.of[IO, Int](0)
        ref2 <- Ref.of[IO, Int](0)
        tracker1 = new Tracker[IO, String] {
          override def displayInitial(target: String, params: Parameters.GA): IO[Unit]                                  = ref1.update(_ + 1)
          override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[String]): IO[Unit] = ref1.update(_ + 1)
          override def displayFinal(population: EvaluatedPopulation[String]): IO[Unit]                                  = ref1.update(_ + 1)
        }
        tracker2 = new Tracker[IO, String] {
          override def displayInitial(target: String, params: Parameters.GA): IO[Unit]                                  = ref2.update(_ + 1)
          override def displayProgress(currentGen: Int, maxGen: Int, population: EvaluatedPopulation[String]): IO[Unit] = ref2.update(_ + 1)
          override def displayFinal(population: EvaluatedPopulation[String]): IO[Unit]                                  = ref2.update(_ + 1)
        }
        composite = CompositeTracker.make[IO, String](tracker1, tracker2)
        _ <- composite.displayInitial("target", params)
        _ <- composite.displayProgress(1, 10, Vector.empty)
        _ <- composite.displayFinal(Vector.empty)
        count1 <- ref1.get
        count2 <- ref2.get
      } yield (count1, count2)

      result.asserting { case (count1, count2) =>
        count1 mustBe 3
        count2 mustBe 3
      }
    }
  }
}
