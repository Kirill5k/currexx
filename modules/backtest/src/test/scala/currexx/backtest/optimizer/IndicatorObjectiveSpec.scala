package currexx.backtest.optimizer

import cats.effect.IO
import currexx.backtest.MarketDataProvider.Corpus
import currexx.backtest.{MarketDataProvider, TestStrategy}
import kirill5k.common.cats.test.IOWordSpec

class IndicatorObjectiveSpec extends IOWordSpec {

  private val strategy = TestStrategy.s12
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

    "refuse a candidate that earned nothing anywhere" in {
      // The only absolute left. Earning in no fold at all is not a shape to be ranked below others, it is nothing.
      IndicatorObjective.FoldAggregation.combine(List(0.0, 0.0, 0.0)) mustBe 0.0
      IndicatorObjective.FoldAggregation.combine(List(0.0)) mustBe 0.0
      IndicatorObjective.FoldAggregation.combine(Nil) mustBe 0.0
    }

    "charge a failed fold heavily without pretending the rest did not happen" in {
      // Until 2026-09-02 every one of these was exactly 0.0, so a candidate that earned in five folds of six ranked
      // level with one that earned in none - and the s1_v2 and s2 lineages, which lose money in search folds 1 to 3 by
      // record, were pinned there by construction. They have to be ordered, and ordered well below a clean sweep.
      val clean     = IndicatorObjective.FoldAggregation.combine(List.fill(6)(1.0))
      val oneShort  = IndicatorObjective.FoldAggregation.combine(0.0 :: List.fill(5)(1.0))
      val twoShort  = IndicatorObjective.FoldAggregation.combine(List.fill(2)(0.0) ++ List.fill(4)(1.0))
      val fiveShort = IndicatorObjective.FoldAggregation.combine(List.fill(5)(0.0) ++ List(1.0))

      clean mustBe 1.0000 +- 0.0001
      oneShort mustBe 0.4580 +- 0.0001
      twoShort mustBe 0.2069 +- 0.0001
      fiveShort mustBe 0.0116 +- 0.0001

      val descending = List(clean, oneShort, twoShort, fiveShort)
      descending.zip(descending.tail).forall((better, worse) => better > worse) mustBe true
    }

    "rank a barely-alive fold above a dead one" in {
      // The regression that sank the first attempt at softening the cliff. Taking the mean over only the folds that
      // earned something and discounting by how many did not scored [1.8, 0.0] at 0.225 and [1.8, 0.001] at 0.042, so
      // the aggregate fell by a factor of five when a fold improved. Every fold has to be worth more alive than dead.
      val dead  = IndicatorObjective.FoldAggregation.combine(List(1.8, 0.0))
      val alive = IndicatorObjective.FoldAggregation.combine(List(1.8, 0.001))
      val awake = IndicatorObjective.FoldAggregation.combine(List(1.8, 0.1))

      alive must be > dead
      awake must be > alive
    }

    "keep a clean sweep ahead of a candidate that earned twice as much but missed a fold" in {
      // The other failure of the filtered version, which scored the five-fold candidate 1.157 against the clean
      // sweep's 1.000 - exactly the single-fitted-regime shape the aggregate exists to refuse.
      val missedOne = IndicatorObjective.FoldAggregation.combine(0.0 :: List.fill(5)(2.0))
      val clean     = IndicatorObjective.FoldAggregation.combine(List.fill(6)(1.0))

      missedOne must be < clean
    }

    "still read the spread among the folds a candidate did earn in" in {
      // The property the product was chosen for, kept under the ramp: having failed the same number of folds, one huge
      // fold must not pay for a weak one.
      val lumpy  = IndicatorObjective.FoldAggregation.combine(List(0.0, 1.8, 0.2))
      val steady = IndicatorObjective.FoldAggregation.combine(List(0.0, 1.0, 1.0))

      lumpy must be < steady
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

      // 0.4024 rather than the unshifted 0.4: `deadFoldFloor` lifts every score a little, and most where the folds are
      // small and uneven. The ordering below is the assertion that matters; the level is recorded to catch a change in
      // the shift itself.
      lumpyPair mustBe 0.4024 +- 0.0001
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
