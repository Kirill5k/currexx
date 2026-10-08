package currexx.backtest

import currexx.algorithms.Parameters
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.optimizer.ScoringFunction
import currexx.domain.signal.Indicator

final case class OptimisationRound(
    name: String,
    strategy: TestStrategy,
    parameters: Parameters.GA | Parameters.SCGA,
    scoringFunction: ScoringFunction,
    corpus: Corpus = MarketDataProvider.majors1hCorpus,
    shortlistSize: Int = 25,
    /** Compatible parameter sets mixed into the starting population alongside the target, always evaluated under the target's rules. Both
      * refinement and exploration divide their clone/jitter allocation among these seeds. The search space filters incompatible schemas,
      * restores fixed values and removes duplicate projections before mixing them.
      */
    extraSeeds: List[NamedIndicator] = Nil,
    /** Additional indicators or composite subtrees to keep at their target values. All identical occurrences are fixed; values absent from
      * the strategy produce a validation error. Raw-close identity inputs and value trackers unused by these rules are fixed automatically.
      */
    fixedIndicators: Set[Indicator] = Set.empty
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
      extraSeeds: List[NamedIndicator] = Nil,
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
      List(NamedIndicator("s2_optimized_v2", TestStrategy.s2_optimized_v2.indicator))
    ),
    Family(
      "s10_optimized",
      TestStrategy.s10_optimized,
      List(NamedIndicator("s10", TestStrategy.s10.indicator))
    ),
    Family(
      "s5_optimized_v2",
      TestStrategy.s5_optimized_v2,
      List(
        NamedIndicator("s5_optimized_v3", TestStrategy.s5_optimized_v3.indicator),
        NamedIndicator("s6", TestStrategy.s6.indicator),
        NamedIndicator("s6_optimized", TestStrategy.s6_optimized.indicator)
      )
    ),
    Family(
      "s4_optimized_v2",
      TestStrategy.s4_optimized_v2,
      List(NamedIndicator("s4_optimized_v1", TestStrategy.s4_optimized_v1.indicator))
    ),
    Family(
      "s6_optimized",
      TestStrategy.s6_optimized,
      List(
        NamedIndicator("s6", TestStrategy.s6.indicator),
        NamedIndicator("s5_optimized_v2", TestStrategy.s5_optimized_v2.indicator),
        NamedIndicator("s5_optimized_v3", TestStrategy.s5_optimized_v3.indicator)
      )
    ),
    Family("s10_v2", TestStrategy.s10_v2, runScga = true),
    Family("s13", TestStrategy.s13, List(NamedIndicator("s13_optimized", TestStrategy.s13_optimized.indicator)), runScga = true),
    Family(
      "s1_v2_optimized",
      TestStrategy.s1_v2_optimized,
      List(NamedIndicator("s1_v2_optimized_v4", TestStrategy.s1_v2_optimized_v4.indicator)),
      runScga = true
    )
  )

  /** Every family receives refining and exploring GA rounds, plus an exploring SCGA round when enabled. `shuffle` changes the initial
    * population mix, not market-data order. Compatible extra seeds contribute parameters only; each round retains its own rules and fixed
    * inputs. All rounds use the standard search/validation corpus, excluding the historical period reused during development.
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
