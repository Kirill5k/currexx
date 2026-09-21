package currexx.backtest

import currexx.algorithms.Parameters
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class OptimiserSpec extends AnyWordSpec with Matchers {
  "Optimiser rounds" should {
    "give every family two GA rounds and add SCGA only for enabled families" in {
      val rounds = Optimiser.rounds
      val byStrategy = rounds.groupBy(_.strategy)
      val scgaStrategies = Set(TestStrategy.s10_v2, TestStrategy.s13, TestStrategy.s1_v2_optimized)

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
        val expected: List[Parameters.GA | Parameters.SCGA] = List(Optimiser.gaParameters, Optimiser.gaParametersWithShuffle) :::
          Option.when(scgaStrategies.contains(strategy))(Parameters.SCGA.from(Optimiser.gaParametersWithShuffle)).toList
        familyRounds.map(_.parameters) mustBe expected
        familyRounds.head.name must endWith("_ga_refine")
        familyRounds(1).name must endWith("_ga_explore")
        familyRounds.drop(2).foreach(_.name must endWith("_scga_explore"))
      }
      Optimiser.gaParameters mustBe Parameters.GA(300, 150, 0.7, 0.1, 0.02, shuffle = false)
      Optimiser.gaParametersWithShuffle mustBe Optimiser.gaParameters.copy(shuffle = true, initialOversampling = 3)
      rounds.foreach { round =>
        round.corpus mustBe MarketDataProvider.majors1hCorpus
        round.scoringFunction mustBe Optimiser.consistentScoring
        round.shortlistSize mustBe 25
        round.fixedIndicators mustBe Set.empty
      }
    }

    "search the retained s10 and reciprocal s6 parameters while retaining separate indicator families" in {
      val rounds = Optimiser.rounds.map(round => round.strategy -> round).toMap
      rounds(TestStrategy.s10_optimized).extraSeeds mustBe List(TestStrategy.s10.indicator)
      rounds(TestStrategy.s6_optimized).extraSeeds mustBe List(
        TestStrategy.s6.indicator, TestStrategy.s5_optimized_v2.indicator, TestStrategy.s5_optimized_v3.indicator
      )
      rounds(TestStrategy.s5_optimized_v2).extraSeeds mustBe List(
        TestStrategy.s5_optimized_v3.indicator, TestStrategy.s6.indicator, TestStrategy.s6_optimized.indicator
      )
      rounds(TestStrategy.s4_optimized_v2).extraSeeds mustBe List(TestStrategy.s4_optimized_v1.indicator)
      rounds(TestStrategy.s10_v2).extraSeeds mustBe Nil
      rounds(TestStrategy.s13).extraSeeds mustBe Nil
    }
  }
}
