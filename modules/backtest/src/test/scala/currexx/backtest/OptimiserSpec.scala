package currexx.backtest

import currexx.algorithms.{Fitness, Parameters}
import currexx.backtest.optimizer.ScoringFunction
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptimiserSpec extends AnyWordSpec with Matchers {
  private val consistentScoring = ScoringFunction.Consistent()
  private val reportRound       = OptimisationRound(
    "verdict-test",
    TestStrategy.s10,
    Optimiser.gaParameters,
    consistentScoring
  )

  private def verdict(validation: Double, breaches: List[ScoringFunction.Violation]): List[String] =
    Optimiser.verdict(reportRound, Vector((reportRound.strategy.indicator, Fitness(1.0), Fitness(validation))), breaches)

  "Optimiser rounds" should {
    "give every family two GA rounds and add SCGA only for enabled families" in {
      val rounds                  = Optimiser.rounds
      val byStrategy              = rounds.groupBy(_.strategy)
      val scgaStrategies          = Set(TestStrategy.s10_v2, TestStrategy.s13, TestStrategy.s1_v2_optimized)
      val gaParametersWithShuffle = Optimiser.gaParameters.copy(shuffle = true, initialOversampling = 3)
      val emptyStats              = List(OrderStats())

      rounds must have size 19
      rounds.map(_.name).distinct must have size 19
      byStrategy.keySet mustBe Set(
        TestStrategy.s2_optimized,
        TestStrategy.s10_optimized,
        TestStrategy.s10_v2,
        TestStrategy.s5_optimized_v2,
        TestStrategy.s4_optimized_v2,
        TestStrategy.s6_optimized,
        TestStrategy.s13,
        TestStrategy.s1_v2_optimized
      )
      byStrategy.foreach { case (strategy, familyRounds) =>
        val expected: List[Parameters.GA | Parameters.SCGA] = List(Optimiser.gaParameters, gaParametersWithShuffle) :::
          Option.when(scgaStrategies.contains(strategy))(Parameters.SCGA.from(gaParametersWithShuffle)).toList
        familyRounds.map(_.parameters) mustBe expected
        familyRounds.head.name must endWith("_ga_refine")
        familyRounds(1).name must endWith("_ga_explore")
        familyRounds.drop(2).foreach(_.name must endWith("_scga_explore"))
      }
      Optimiser.gaParameters mustBe Parameters.GA(300, 150, 0.7, 0.1, 0.02, shuffle = false)
      rounds.foreach { round =>
        round.corpus mustBe MarketDataProvider.majors1hCorpus
        round.scoringFunction.score(emptyStats) mustBe consistentScoring.score(emptyStats)
        round.scoringFunction.violations(emptyStats) mustBe consistentScoring.violations(emptyStats)
        round.shortlistSize mustBe 25
        round.fixedIndicators mustBe Set.empty
      }
    }

    "search the retained s10 and reciprocal s6 parameters while retaining separate indicator families" in {
      val rounds = Optimiser.rounds.map(round => round.strategy -> round).toMap
      rounds(TestStrategy.s10_optimized).extraSeeds mustBe List(TestStrategy.s10.indicator)
      rounds(TestStrategy.s6_optimized).extraSeeds mustBe List(
        TestStrategy.s6.indicator,
        TestStrategy.s5_optimized_v2.indicator,
        TestStrategy.s5_optimized_v3.indicator
      )
      rounds(TestStrategy.s5_optimized_v2).extraSeeds mustBe List(
        TestStrategy.s5_optimized_v3.indicator,
        TestStrategy.s6.indicator,
        TestStrategy.s6_optimized.indicator
      )
      rounds(TestStrategy.s4_optimized_v2).extraSeeds mustBe List(TestStrategy.s4_optimized_v1.indicator)
      rounds(TestStrategy.s10_v2).extraSeeds mustBe Nil
      rounds(TestStrategy.s13).extraSeeds mustBe Nil
    }
  }

  "Optimiser verdict" should {
    "explain every constraint breach when the leading finalist scores zero" in {
      val breaches = List(
        ScoringFunction.Violation("median period profit", "-1.25", "> 0"),
        ScoringFunction.Violation("closed trades", "60", ">= 120")
      )
      val lines = verdict(0.0, breaches)

      lines.exists(_.startsWith("NOTHING SELECTED:")) mustBe true
      lines must contain("BREACHES 2 constraint(s) on validation data:")
      breaches.foreach(breach => lines must contain(s"  - $breach"))
      lines.mkString("\n") must include("Leading finalist, recorded for diagnostics only:")
      (lines.mkString("\n") must not).include("did not find an edge")
    }

    "retain constraint failures for a positive validation score" in {
      val breach = ScoringFunction.Violation("closed trades", "60", ">= 120")
      val lines  = verdict(0.5, List(breach))

      lines.exists(_.startsWith("SELECTED (")) mustBe true
      lines must contain("BREACHES 1 constraint(s) on validation data:")
      lines must contain(s"  - $breach")
    }

    "retain the successful constraint check when there are no breaches" in {
      verdict(0.5, Nil) must contain("Satisfies every constraint on validation data.")
    }
  }
}
