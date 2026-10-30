package currexx.backtest

import currexx.algorithms.{Fitness, Parameters}
import currexx.backtest.optimizer.ScoringFunction
import currexx.backtest.optimizer.{NamedStrategy, OptimisationRound, OptimisationRounds, OptimisationSearchSpace, OptimisationTarget}
import currexx.backtest.optimizer.reporting.OptimisationReportRenderer
import currexx.backtest.optimizer.OptimisationSearchSpace.SeedDisposition
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
    OptimisationReportRenderer.verdict(reportRound, Vector((reportRound.strategy, Fitness(1.0), Fitness(validation))), breaches)

  "Optimiser rounds" should {

    "default every scheduled round to joint search with valid applicable rule bounds" in {
      reportRound.target mustBe OptimisationTarget.Both
      OptimisationRounds.rounds.foreach { round =>
        withClue(s"${round.name}: ") {
          round.target mustBe OptimisationTarget.Both
          round.ruleBounds mustBe currexx.backtest.optimizer.strategy.RuleParameterBounds.default
          OptimisationRounds.findByName(round.name) mustBe Right(round)
          val search = OptimisationSearchSpace.forRound(round).fold(error => fail(error.getMessage), identity)
          search.target mustBe OptimisationTarget.Both
          search.indicatorParameterCount must be > 0
          val ruleSearchFamilies = Set(
            TestStrategy.s10_optimized,
            TestStrategy.s10_v2,
            TestStrategy.s4_optimized_v2,
            TestStrategy.s12_optimized
          )
          search.strategyParameterCount mustBe (if (ruleSearchFamilies.contains(round.strategy)) 1 else 0)
          val resolved = search.resolveSeeds(round.extraSeeds.map(_.strategy)).toOption.get
          resolved.map(_.disposition) mustBe List.fill(round.extraSeeds.size)(SeedDisposition.Accepted)
        }
      }
      succeed
    }

    "resolve a named preset and return an error for an unknown name" in {
      OptimisationRounds.findByName("s13_ga_refine").map(_.strategy) mustBe Right(TestStrategy.s13)
      OptimisationRounds.roundsByName("s13_ga_refine").parameters mustBe Optimiser.gaParameters
      OptimisationRounds.findByName("s12_optimized_ga_refine").map(_.strategy) mustBe Right(TestStrategy.s12_optimized)
      OptimisationRounds.findByName("s12_optimized_ga_explore").map(_.strategy) mustBe Right(TestStrategy.s12_optimized)
      OptimisationRounds.findByName("s12_optimized_scga_explore").map(_.strategy) mustBe Right(TestStrategy.s12_optimized)
      val missing = OptimisationRounds.findByName("unknown-round").swap.toOption.get
      missing.getMessage must include("Unknown round 'unknown-round'")
      missing.getMessage must include("s13_ga_refine")
    }

    "retain all s5 seed indicators under the configured base rules" in {
      val round    = OptimisationRounds.findByName("s5_optimized_v2_ga_refine").toOption.get
      val search   = OptimisationSearchSpace.forRound(round).toOption.get
      val resolved = search.resolveSeeds(round.extraSeeds.map(_.strategy)).toOption.get
      resolved.map(_.disposition) mustBe List.fill(3)(SeedDisposition.Accepted)
      resolved.map(_.usedBaseRules) mustBe List(false, true, true)
      resolved.foreach(_.effective.get.rules mustBe round.strategy.rules)
      resolved.flatMap(_.effective).map(_.indicator).distinct must have size 3
    }

    "give every family two GA rounds and add SCGA only for enabled families" in {
      val rounds         = Optimiser.rounds
      val byStrategy     = rounds.groupBy(_.strategy)
      val scgaStrategies = Set(
        TestStrategy.s10_v2,
        TestStrategy.s13,
        TestStrategy.s1_v2_optimized,
        TestStrategy.s12_optimized
      )
      val gaParametersWithShuffle = Optimiser.gaParameters.copy(shuffle = true, initialOversampling = 3)
      val emptyStats              = List(OrderStats())

      rounds must have size 22
      rounds.map(_.name).distinct must have size 22
      byStrategy.keySet mustBe Set(
        TestStrategy.s2_optimized,
        TestStrategy.s10_optimized,
        TestStrategy.s10_v2,
        TestStrategy.s5_optimized_v2,
        TestStrategy.s4_optimized_v2,
        TestStrategy.s6_optimized,
        TestStrategy.s13,
        TestStrategy.s1_v2_optimized,
        TestStrategy.s12_optimized
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

    "search retained alternatives and reciprocal s6 parameters while retaining separate indicator families" in {
      val rounds = Optimiser.rounds.map(round => round.strategy -> round).toMap
      rounds(TestStrategy.s2_optimized).extraSeeds mustBe List(NamedStrategy("s2_optimized_v2", TestStrategy.s2_optimized_v2))
      rounds(TestStrategy.s10_optimized).extraSeeds mustBe List(NamedStrategy("s10", TestStrategy.s10))
      rounds(TestStrategy.s6_optimized).extraSeeds mustBe List(
        NamedStrategy("s6", TestStrategy.s6),
        NamedStrategy("s5_optimized_v2", TestStrategy.s5_optimized_v2),
        NamedStrategy("s5_optimized_v3", TestStrategy.s5_optimized_v3)
      )
      rounds(TestStrategy.s5_optimized_v2).extraSeeds mustBe List(
        NamedStrategy("s5_optimized_v3", TestStrategy.s5_optimized_v3),
        NamedStrategy("s6", TestStrategy.s6),
        NamedStrategy("s6_optimized", TestStrategy.s6_optimized)
      )
      rounds(TestStrategy.s4_optimized_v2).extraSeeds mustBe List(NamedStrategy("s4_optimized_v1", TestStrategy.s4_optimized_v1))
      rounds(TestStrategy.s10_v2).extraSeeds mustBe Nil
      rounds(TestStrategy.s13).extraSeeds mustBe List(NamedStrategy("s13_optimized", TestStrategy.s13_optimized))
      rounds(TestStrategy.s1_v2_optimized).extraSeeds mustBe List(
        NamedStrategy("s1_v2_optimized_v2", TestStrategy.s1_v2_optimized_v2)
      )
      rounds(TestStrategy.s12_optimized).extraSeeds mustBe List(NamedStrategy("s12", TestStrategy.s12))

      val s12Search = OptimisationSearchSpace.forRound(rounds(TestStrategy.s12_optimized)).toOption.get
      val s12Seed   = s12Search.resolveSeeds(List(TestStrategy.s12)).toOption.get.head
      s12Seed.effective mustBe Some(TestStrategy.s12)
      s12Seed.usedBaseRules mustBe false

      List(TestStrategy.s5_optimized_v2, TestStrategy.s6_optimized).foreach { strategy =>
        val round     = rounds(strategy)
        val both      = OptimisationSearchSpace.forRound(round).toOption.get
        val indicator = OptimisationSearchSpace.forRound(round.copy(target = OptimisationTarget.Indicator)).toOption.get
        val seeds     = round.extraSeeds.map(_.strategy)
        val resolved  = both.resolveSeeds(seeds).toOption.get
        resolved.map(_.disposition) mustBe List.fill(3)(SeedDisposition.Accepted)
        resolved.map(_.effective) mustBe indicator.resolveSeeds(seeds).toOption.get.map(_.effective)
        resolved.map(_.usedBaseRules) mustBe List(false, true, true)
        resolved.filter(_.usedBaseRules).foreach { seed =>
          seed.effective.get.rules mustBe strategy.rules
          seed.reason.get must include("Using seed indicators with base rules")
        }
      }
    }
  }

  "Optimisation report verdict" should {
    "explain every constraint breach when the leading finalist scores zero" in {
      val breaches = List(
        ScoringFunction.Violation("median period profit", "-1.25", "> 0"),
        ScoringFunction.Violation("closed trades", "60", ">= 120")
      )
      val lines = verdict(0.0, breaches)

      lines.exists(_.startsWith("SEARCH LEADER:")) mustBe true
      lines must contain("BREACHES 2 constraint(s) on validation data:")
      breaches.foreach(breach => lines must contain(s"  - $breach"))
      lines.mkString("\n") must include("Ranking is diagnostic.")
      (lines.mkString("\n") must not).include("did not find an edge")
    }

    "retain constraint failures for a positive validation score" in {
      val breach = ScoringFunction.Violation("closed trades", "60", ">= 120")
      val lines  = verdict(0.5, List(breach))

      lines.exists(_.startsWith("SEARCH LEADER:")) mustBe true
      lines must contain("BREACHES 1 constraint(s) on validation data:")
      lines must contain(s"  - $breach")
    }

    "retain the successful constraint check when there are no breaches" in {
      verdict(0.5, Nil) must contain("Satisfies every constraint on validation data.")
    }
  }
}
