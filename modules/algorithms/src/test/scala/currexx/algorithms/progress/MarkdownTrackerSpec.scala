package currexx.algorithms.progress

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.{Fitness, Parameters}
import fs2.io.file.{Files, Path}

class MarkdownTrackerSpec extends IOWordSpec {

  "MarkdownTracker" should {
    val params     = Parameters.GA(100, 10, 0.5, 0.1, 0.1, true)
    val population = Vector(
      ("ind1", Fitness(10.0)),
      ("ind2", Fitness(5.0))
    )
    // The final report is the one place both fitnesses exist, so it renders the pair rather than the training figure
    // the progress lines carry, and orders on the validation one.
    val validated = Vector(
      ("ind1", Fitness(10.0), Fitness(4.0)),
      ("ind2", Fitness(5.0), Fitness(1.0))
    )

    "write progress to a markdown file in optimisation-results folder" in {
      val resultsDir = Path("optimisation-results")
      val filesS     = Files.forAsync[IO]

      val result = for {
        tracker <- MarkdownTracker.make[IO, String](logInterval = 1, showStats = true)
        _       <- tracker.displayInitial("target-ind", params)
        _       <- tracker.displayProgress(1, 10, population)
        _       <- tracker.displayFinal(validated)
        _     <- tracker.displayNote("Champion: round-1", List("Fitness: 10.0", "BREACHES 1 constraint(s) despite winning:", "  - too few"))
        files <- filesS.list(resultsDir).compile.toList
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
        content must include("2 finalist(s) validated, 0 of which scored zero")
        content must include("rank  train#    training  validation  retained  individual")
        content must include("1       1   10.000000    4.000000     40.0%  ind1")
        content must include("Stats: Best=4.0, Avg=2.5, Worst=1.0")
        content must include("## Champion: round-1")
        content must include("BREACHES 1 constraint(s) despite winning:\n  - too few")
      }
    }
  }
}
