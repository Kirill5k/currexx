package currexx.algorithms.progress

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.{Fitness, Parameters}
import fs2.io.file.{Files, Path}

class MarkdownTrackerSpec extends IOWordSpec {

  "MarkdownTracker" should {
    val params = Parameters.GA(100, 10, 0.5, 0.1, 0.1, true)
    val population = Vector(
      ("ind1", Fitness(10.0)),
      ("ind2", Fitness(5.0))
    )

    "write progress to a markdown file in optimisation-results folder" in {
      val resultsDir = Path("optimisation-results")
      val filesS     = Files.forAsync[IO]

      val result = for {
        tracker <- MarkdownTracker.make[IO, String](logInterval = 1, showStats = true)
        _       <- tracker.displayInitial("target-ind", params)
        _       <- tracker.displayProgress(1, 10, population)
        _       <- tracker.displayFinal(population)
        files   <- filesS.list(resultsDir).compile.toList
        latestFile = files.filter(_.fileName.toString.startsWith("ga-optimisation-")).maxBy(_.toString)
        content <- filesS.readUtf8(latestFile).compile.string
        _       <- filesS.deleteIfExists(latestFile)
      } yield content

      result.asserting { content =>
        content must include("# Genetic Algorithm Run")
        content must include("## Progress")
        content must include("### Generation 1 out of 10")
        content must include("* #1: 10.0 - `ind1`")
        content must include("## Final Results")
        content must include("Stats: Best=10.0, Avg=7.5, Worst=5.0")
      }
    }
  }
}
