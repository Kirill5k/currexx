package currexx.backtest.optimizer

import currexx.algorithms.Parameters
import currexx.backtest.{MarketDataProvider, TestStrategy}
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.strategy.*
import currexx.domain.signal.Indicator

final case class OptimisationRound(
    name: String,
    strategy: TestStrategy,
    parameters: Parameters.GA | Parameters.SCGA,
    scoringFunction: ScoringFunction,
    corpus: Corpus = MarketDataProvider.majors1hCorpus,
    shortlistSize: Int = 25,
    /** Named complete seeds. The target restores inactive components before validation and deduplication. */
    extraSeeds: List[NamedStrategy] = Nil,
    /** Additional indicators or composite subtrees to keep at their target values. All identical occurrences are fixed; values absent from
      * the strategy produce a validation error. Raw-close identity inputs and value trackers unused by these rules are fixed automatically.
      */
    fixedIndicators: Set[Indicator] = Set.empty,
    searchObjective: SearchObjectiveConfig = SearchObjectiveConfig.Current,
    upgradePolicy: UpgradePolicyConfig = UpgradePolicyConfig(),
    target: OptimisationTarget = OptimisationTarget.Both,
    /** Named bounds apply only where that condition exists. Indicator-only rounds ignore rule bounds. */
    ruleBounds: RuleParameterBounds = RuleParameterBounds.default
)

/** Shared search presets, independent of either entry point's execution and random generator. */
object OptimisationRounds:
  val gaParameters = Parameters.GA(
    populationSize = 300,
    maxGen = 150,
    crossoverProbability = 0.7,
    mutationProbability = 0.1,
    elitismRatio = 0.02,
    shuffle = false
  )

  final private case class Family(
      name: String,
      strategy: TestStrategy,
      extraSeeds: List[NamedStrategy] = Nil,
      // Include one additional SCGA exploration round.
      runScga: Boolean = false
  ):
    def toRound(params: Parameters.GA | Parameters.SCGA, mode: String): OptimisationRound =
      OptimisationRound(
        name = s"${name}_${params.name.toLowerCase}_$mode",
        strategy = strategy,
        parameters = params,
        scoringFunction = ScoringFunction.Consistent(),
        extraSeeds = extraSeeds
      )

  private val families = List(
    Family(
      "s2_optimized",
      TestStrategy.s2_optimized,
      List(NamedStrategy("s2_optimized_v2", TestStrategy.s2_optimized_v2))
    ),
    Family(
      "s10_optimized",
      TestStrategy.s10_optimized,
      List(NamedStrategy("s10", TestStrategy.s10))
    ),
    Family(
      "s5_optimized_v2",
      TestStrategy.s5_optimized_v2,
      List(
        NamedStrategy("s5_optimized_v3", TestStrategy.s5_optimized_v3),
        NamedStrategy("s6", TestStrategy.s6),
        NamedStrategy("s6_optimized", TestStrategy.s6_optimized)
      )
    ),
    Family(
      "s4_optimized_v2",
      TestStrategy.s4_optimized_v2,
      List(NamedStrategy("s4_optimized_v1", TestStrategy.s4_optimized_v1))
    ),
    Family(
      "s6_optimized",
      TestStrategy.s6_optimized,
      List(
        NamedStrategy("s6", TestStrategy.s6),
        NamedStrategy("s5_optimized_v2", TestStrategy.s5_optimized_v2),
        NamedStrategy("s5_optimized_v3", TestStrategy.s5_optimized_v3)
      )
    ),
    Family("s10_v2", TestStrategy.s10_v2, runScga = true),
    Family("s13", TestStrategy.s13, List(NamedStrategy("s13_optimized", TestStrategy.s13_optimized)), runScga = true),
    Family(
      "s1_v2_optimized",
      TestStrategy.s1_v2_optimized,
      List(NamedStrategy("s1_v2_optimized_v4", TestStrategy.s1_v2_optimized_v4)),
      runScga = true
    ),
    Family(
      "s12",
      TestStrategy.s12,
      List(NamedStrategy("s12_optimized", TestStrategy.s12_optimized)),
      runScga = true
    )
  )

  /** Every family receives refining and exploring GA rounds, plus an exploring SCGA round when enabled. `shuffle` changes the initial
    * population mix, not market-data order. Extra seeds contain complete strategies. The target selects their active components and
    * restores inactive components and fixed inputs from the base. All rounds use the standard search/validation corpus, excluding the
    * historical period reused during development.
    */
  val rounds: List[OptimisationRound] = families.flatMap { family =>
    // Exploration draws three populations and retains the best population's worth of members.
    // SCGA.from preserves these settings; its species defaults remain unchanged for the comparison.
    val gaParametersWithShuffle = gaParameters.copy(shuffle = true, initialOversampling = 3)
    family.toRound(gaParameters, "refine")
      :: family.toRound(gaParametersWithShuffle, "explore")
      :: Option.when(family.runScga)(family.toRound(Parameters.SCGA.from(gaParametersWithShuffle), "explore")).toList
  }

  val roundsByName: Map[String, OptimisationRound] = rounds.map(round => round.name -> round).toMap

  def findByName(name: String): Either[IllegalArgumentException, OptimisationRound] =
    roundsByName
      .get(name)
      .toRight(
        new IllegalArgumentException(s"Unknown round '$name'. Available rounds: ${rounds.map(_.name).mkString(", ")}")
      )
