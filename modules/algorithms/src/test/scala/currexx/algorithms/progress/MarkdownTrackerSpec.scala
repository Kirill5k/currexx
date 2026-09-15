package currexx.algorithms.progress

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.operators.species.SpeciesStats
import currexx.algorithms.{Fitness, Parameters}
import fs2.io.file.{Files, Path}

import java.util.UUID

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
      val label      = s"markdown-test-${UUID.randomUUID()}"

      val result = for {
        tracker <- Tracker.markdown[IO, String](algorithmName = params.name, label = label, logInterval = 1, showStats = true)
        _       <- tracker.displayInitial("target-ind", params)
        _       <- tracker.displayProgress(Progress.Population(1, 10, population))
        _       <- tracker.displayFinal(validated)
        _     <- tracker.displayNote("Champion: round-1", List("Fitness: 10.0", "BREACHES 1 constraint(s) despite winning:", "  - too few"))
        files <- filesS.list(resultsDir).compile.toList
        latestFile = files.find(_.fileName.toString.endsWith(s"-$label.md")).get
        content <- filesS.readUtf8(latestFile).compile.string
        _       <- filesS.deleteIfExists(latestFile)
      } yield (latestFile, content)

      result.asserting { case (path, content) =>
        path.fileName.toString must startWith("ga-optimisation-")
        content must include("# Genetic Algorithm Run")
        (content must not).include("SCGA")
        (content must not).include("Breeding species")
        content must include("## Progress")
        content must include("### Generation 1 out of 10")
        content.linesIterator.count(_.startsWith("### Generation")) mustBe 1
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

    "write one SCGA generation report and apply the interval to its population and breeding telemetry together" in {
      val resultsDir        = Path("optimisation-results")
      val filesS            = Files.forAsync[IO]
      val label             = s"scga-markdown-test-${UUID.randomUUID()}"
      val scgaParams        = Parameters.SCGA(10, 20, 0.5, 0.1, shuffle = true)
      val stats             = SpeciesStats(Vector(6, 4), Vector(5, 3), 8)
      val skippedPopulation = Vector("skipped-candidate" -> Fitness(99.0))
      val skippedStats      = SpeciesStats(Vector(9, 1), Vector(7, 1), 3)

      val result = for {
        tracker <- Tracker.markdown[IO, String](algorithmName = scgaParams.name, label = label, logInterval = 2, showStats = true)
        _       <- tracker.displayInitial("scga-target", scgaParams)
        _       <- tracker.displayProgress(Progress.Species(1, 20, skippedPopulation, skippedStats))
        _       <- tracker.displayProgress(Progress.Species(2, 20, population, stats))
        _       <- tracker.displayFinal(validated)
        _       <- tracker.displayNote("Champion after final", List("report still open"))
        files   <- filesS.list(resultsDir).compile.toList
        reports = files.filter(_.fileName.toString.endsWith(s"-$label.md"))
        content <- filesS.readUtf8(reports.head).compile.string
        _       <- filesS.deleteIfExists(reports.head)
      } yield (reports, content)

      result.asserting { case (reports, content) =>
        reports.size mustBe 1
        reports.head.fileName.toString must startWith("scga-optimisation-")
        content must include("# Species-Conserving Genetic Algorithm Run")
        content must include(s"**Parameters:** $scgaParams")
        (content must not).include("Generation 1 out of 20")
        (content must not).include("skipped-candidate")
        (content must not).include("Parent sizes=[9, 1]")
        (content must not).include("Stats: Best=99.0")
        content must include("### Generation 2 out of 20")
        content must include("* #1: 10.0 - `ind1`")
        content must include("Stats: Best=10.0, Avg=7.5, Worst=5.0")
        content must include("Breeding species: Count=2, Parent sizes=[6, 4], Offspring=[5, 3], Distinct parents=8")
        content.linesIterator.count(_.startsWith("### Generation")) mustBe 1
        content.linesIterator.count(_.startsWith("Breeding species:")) mustBe 1
        content must include("## Final Results")
        content must include("## Champion after final")
        content must include("report still open")
        content.indexOf("## Champion after final") must be > content.indexOf("## Final Results")
      }
    }
  }
}
