package currexx.algorithms

import kirill5k.common.cats.test.IOWordSpec

class SCGAMultimodalSpec extends IOWordSpec {
  "SCGA on separated fitness peaks" should {
    "conserve both promising basins while improving their representatives with real species operators" in
      MultimodalExperiment.run(seed = 7L, scga = true, maxGen = 40).asserting { result =>
        result.generations.size mustBe 40
        result.generations.foreach { population =>
          population.size mustBe 40
          population.exists(_._1 < 0.0) mustBe true
          population.exists(_._1 > 0.0) mustBe true
        }
        MultimodalExperiment.Peaks.foreach { peak =>
          val initialBest = result.initial.filter(_ * peak > 0.0).map(MultimodalExperiment.fitness(_).value).max
          val finalBest   = result.finalPopulation.filter(_._1 * peak > 0.0).map(_._2.value).max
          finalBest must be > (initialBest + 1.0)
        }
        result.peaksRetained mustBe 2
        result.objectiveCalls must be < result.requests
      }

    "reproduce populations and evaluation counts with the same random seed" in {
      val result = for
        first  <- MultimodalExperiment.run(seed = 19L, scga = true, maxGen = 12)
        second <- MultimodalExperiment.run(seed = 19L, scga = true, maxGen = 12)
      yield (first, second)

      result.asserting { case (first, second) =>
        first.initial mustBe second.initial
        first.generations mustBe second.generations
        first.finalPopulation mustBe second.finalPopulation
        first.requests mustBe second.requests
        first.objectiveCalls mustBe second.objectiveCalls
      }
    }
  }
}
