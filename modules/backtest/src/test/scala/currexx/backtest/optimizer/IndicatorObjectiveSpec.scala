package currexx.backtest.optimizer

import cats.effect.IO
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.{MarketDataProvider, TestStrategy}
import kirill5k.common.cats.test.IOWordSpec

class IndicatorObjectiveSpec extends IOWordSpec {

  private val strategy = TestStrategy.s1_v2_optimized_v2
  private val scoring  = ScoringFunction.Robust()

  /** The real split narrowed to one pair per segment, so these cases turn on the fold structure rather than on six files of each. */
  private val corpus = Corpus(
    searchFolds = MarketDataProvider.majors1hCorpus.searchFolds.map(fold => List(fold.head)),
    validationFold = List(MarketDataProvider.majors1hCorpus.validationFold.head)
  )

  "IndicatorObjective.make" should {

    "hand back a backtest that reproduces the run its fitness came from" in {
      val result = for
        objective <- IndicatorObjective.make[IO](
          corpus = corpus,
          strategy = strategy.rules,
          poolSize = 1,
          shortlistSize = 25,
          scoringFunction = scoring
        )
        scored <- objective.evaluator.evaluateIndividual(strategy.indicator)
        stats  <- objective.backtest(strategy.indicator)
      yield (scored._2.value, IndicatorObjective.FoldAggregation.combine(stats.map(scoring.score)))

      // The champion report re-runs the winner through this backtest to say whether it satisfies its constraints. If
      // the replay were configured even slightly differently from the search it would be describing a different run,
      // and the report would be confidently wrong about the candidate that is actually about to be used. Aggregating
      // the replay the same way is part of that: the fitness is the folds combined, so a replay that reported one
      // fold, or combined them differently, would not be the number selection sorted on.
      result.asserting { case (searched, replayed) =>
        replayed mustBe searched
      }
    }

    "score every search fold rather than only the first" in {
      // The whole of what folds buy is that a candidate has to hold up across several disjoint stretches, which is
      // worth nothing if the evaluator quietly reads one of them. Each fold has to come back as its own run, over its
      // own months, in the order the corpus offers them.
      val result = for
        objective <- IndicatorObjective.make[IO](
          corpus = corpus,
          strategy = strategy.rules,
          poolSize = 1,
          shortlistSize = 25,
          scoringFunction = scoring
        )
        stats <- objective.backtest(strategy.indicator)
      yield stats.flatMap(_.flatMap(_.dataWindow))

      result.asserting { windows =>
        windows must have size corpus.foldCount
        windows.sliding(2).foreach {
          case List(earlier, later) => earlier.to.isBefore(later.from) mustBe true
          case _                    => succeed
        }
        succeed
      }
    }

    "run the searched and the validated backtest over their own segments of the data" in {
      // The point of the split is that a champion is ranked on months no candidate was ever scored against, so these
      // two have to disagree about what period they are describing. Sharing a services pool is what makes that worth
      // asserting: were the validation run to inherit any of the search runs' data the two would silently converge on
      // the same answer, and selection would be ranking finalists on the sample that produced them.
      val result = for
        objective <- IndicatorObjective.make[IO](
          corpus = corpus,
          strategy = strategy.rules,
          poolSize = 1,
          shortlistSize = 25,
          scoringFunction = scoring
        )
        trained   <- objective.backtest(strategy.indicator)
        validated <- objective.validate(strategy.indicator)
      yield (trained.flatMap(_.flatMap(_.dataWindow)), validated.flatMap(_.dataWindow))

      result.asserting { case (trained, validated) =>
        validated must have size 1
        trained.map(_.to).max.isBefore(validated.head.from) mustBe true
      }
    }
  }

  "IndicatorObjective.FoldAggregation" should {

    "refuse a candidate that earned nothing in one of its folds" in {
      // The single-fitted-regime shape the aggregate exists to refuse: everything made in one stretch of market and
      // nothing in the others. A fold worth nothing makes the whole worth nothing, whatever the rest managed.
      IndicatorObjective.FoldAggregation.combine(List(1.8, 0.0, 0.0)) mustBe 0.0
      IndicatorObjective.FoldAggregation.combine(List(1.8, 0.0)) mustBe 0.0
      IndicatorObjective.FoldAggregation.combine(List(0.0, 1.8)) mustBe 0.0
    }

    "prefer the balanced of two candidates that earned the same on average, at two folds as at three" in {
      // Two folds is the case that matters, because it is the fewest the corpus can be cut into and it is where a
      // counting statistic over folds stops discriminating: the profitable share can only be nought, a half or one,
      // and a half of a fourfold advantage still wins. Both pairs below share an arithmetic mean, so only a rule that
      // reads the spread can separate them.
      val lumpyPair    = IndicatorObjective.FoldAggregation.combine(List(0.8, 0.2))
      val balancedPair = IndicatorObjective.FoldAggregation.combine(List(0.5, 0.5))
      val lumpyTriple  = IndicatorObjective.FoldAggregation.combine(List(1.0, 0.4, 0.1))
      val steadyTriple = IndicatorObjective.FoldAggregation.combine(List(0.5, 0.5, 0.5))

      lumpyPair mustBe 0.4 +- 0.0001
      balancedPair mustBe 0.5 +- 0.0001
      lumpyPair must be < balancedPair
      lumpyTriple must be < steadyTriple
    }

    "never let a single outstanding fold buy its way past a steady candidate" in {
      // The regression this replaced: discounting the mean by the share of profitable folds scored the lumpy pair
      // 0.675 against the steady pair's 0.450, so the shape being refused won the ranking outright at two folds. The
      // earlier test only covered three folds, where the same rule happened to work, which is how it went unnoticed.
      val lumpy  = IndicatorObjective.FoldAggregation.combine(List(1.8, 0.0))
      val steady = IndicatorObjective.FoldAggregation.combine(List(0.45, 0.45))

      lumpy must be < steady
    }

    "keep ranking candidates that are positive everywhere" in {
      // The cliff is only reached on a fold worth exactly nothing, which ScoringFunction reserves for the genuinely
      // disqualifying. Everywhere above it the aggregate has to stay strictly increasing, or selection loses the
      // gradient it climbs.
      val scores = List(List(0.2, 0.2), List(0.2, 0.4), List(0.4, 0.4), List(0.4, 0.8)).map(IndicatorObjective.FoldAggregation.combine)

      scores.foreach(_ must be > 0.0)
      scores mustBe scores.sorted
      scores.distinct must have size scores.size
    }

    "score an empty result as worthless rather than dividing by nothing" in {
      IndicatorObjective.FoldAggregation.combine(Nil) mustBe 0.0
    }
  }
}
