package currexx.backtest.optimizer

import currexx.backtest.{CompletedTrade, DataWindow, OrderStats, RiskSettings}
import currexx.backtest.types.given
import currexx.domain.market.TradeOrder.Position
import currexx.domain.market.{Currency, CurrencyPair}
import eu.timepit.refined.types.numeric.PosBigDecimal
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*
import scala.language.implicitConversions

class ScoringFunctionSpec extends AnyWordSpec with Matchers {

  private val start = Instant.parse("2025-01-01T00:00:00Z")
  private val pairs = List(
    CurrencyPair(Currency.EUR, Currency.USD),
    CurrencyPair(Currency.GBP, Currency.USD),
    CurrencyPair(Currency.USD, Currency.CAD)
  )

  private def statsFor(
      pair: CurrencyPair,
      netProfits: List[BigDecimal],
      costPerTrade: BigDecimal = BigDecimal(0),
      initialBalance: BigDecimal = BigDecimal(10000),
      invalidOrderCount: Int = 0,
      // Wide enough by default that every trade lands in its own calendar month, which is what gives the monthly
      // return series something to measure. Narrow it to pack a run of trades into a single month.
      spacing: FiniteDuration = 32.days,
      // Absent by default, which leaves anything measured per month falling back on the span of the trades. Supply one
      // to say which months the run was actually offered.
      dataWindow: Option[DataWindow] = None
  ): OrderStats = {
    val trades = netProfits.zipWithIndex.map { case (netProfit, index) =>
      val closedAt = start.plusSeconds(index.toLong * spacing.toSeconds)
      CompletedTrade(
        currencyPair = pair,
        position = Position.Buy,
        openedAt = closedAt.minusSeconds(1.hour.toSeconds),
        closedAt = closedAt,
        entryPrice = BigDecimal(1),
        exitPrice = BigDecimal(1),
        volume = BigDecimal("0.1"),
        grossProfit = netProfit + costPerTrade,
        costs = costPerTrade,
        netProfit = netProfit
      )
    }
    OrderStats.fromTrades(
      trades = trades,
      settings = RiskSettings(initialBalance = PosBigDecimal.unsafeFrom(initialBalance)),
      invalidOrderCount = invalidOrderCount,
      dataWindow = dataWindow
    )
  }

  /** A window covering the twelve calendar months from `start`. */
  private val fullYear = Some(DataWindow(start, start.plusSeconds(360L * 24 * 60 * 60)))

  /** Stats built from trades closing at times given outright, for the cases that turn on where a close lands. */
  private def statsClosingAt(
      pair: CurrencyPair,
      trades: List[(BigDecimal, Instant)],
      dataWindow: Option[DataWindow]
  ): OrderStats =
    OrderStats.fromTrades(
      trades = trades.map { case (netProfit, closedAt) =>
        CompletedTrade(
          currencyPair = pair,
          position = Position.Buy,
          openedAt = closedAt.minusSeconds(1.hour.toSeconds),
          closedAt = closedAt,
          entryPrice = BigDecimal(1),
          exitPrice = BigDecimal(1),
          volume = BigDecimal("0.1"),
          grossProfit = netProfit,
          costs = BigDecimal(0),
          netProfit = netProfit
        )
      },
      settings = RiskSettings(),
      dataWindow = dataWindow
    )

  /** Trades closing ten to the month, which is the rate both scoring functions ask for by default.
    *
    * `statsFor` spaces trades evenly, so its default of one a month puts every fixture built that way permanently below the trade floor.
    * Cases about anything other than sample size need to clear it, and clearing it means trading at the rate rather than reaching some
    * total: `months * 10` trades three days apart.
    */
  private def statsAtFloorRate(pair: CurrencyPair, monthlyProfit: BigDecimal, months: Int): OrderStats =
    statsFor(pair, List.fill(months * 10)(monthlyProfit / 10), spacing = 3.days)

  private val permissiveConfig = ScoringFunction.Robust.Config(
    minTradesPerMonth = 1,
    minProfitableDatasetRatio = 1.0,
    maxDrawdownPercent = 100.0
  )

  "ScoringFunction.Robust" should {
    "return zero for no datasets" in {
      ScoringFunction.Robust().score(Nil) mustBe 0.0
    }

    "assign positive fitness to a robust candidate" in {
      val stats = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))

      ScoringFunction.Robust().score(stats) must be > 0.0
    }

    "credit metrics whose denominator is undefined with their target rather than zero" in {
      val stats = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))

      // Never a drawdown, never a losing month and never a losing trade, so recovery factor, Sortino and
      // expectancy-to-loss are all undefined and each is worth exactly its target. That leaves net return (150
      // trades of 10 against a pooled balance of 30000, so half of the 0.1 target) as the only component below
      // full marks: 0.35 * 0.5 + 0.30 + 0.175 + 0.175.
      ScoringFunction.Robust().score(stats) mustBe 0.825 +- 0.0001
    }

    "withhold credit for a Sortino ratio that could never be measured" in {
      val scoring = ScoringFunction.Robust()
      val spread  = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))
      val burst   = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10)), spacing = 1.hour))

      // Identical trades, profit and drawdown; the only difference is that the burst closed all 150 of them inside one
      // calendar month, leaving no monthly series to measure downside across. Paying that as generously as a year
      // without a losing month buys evidence that was never produced. Trading rarely is already covered by the
      // sample-size penalty, but trading in a burst is not, so the two Nones cannot share a fallback.
      scoring.score(burst) mustBe scoring.score(spread) - 0.175 +- 0.0001
    }

    "discount the credit for an undefined metric when the sample behind it is thin" in {
      // Spread thin rather than simply few: 90 closed trades across twelve months is half the 180 that rate asks of
      // three pairs over a run that long, so each of the three undefined metrics is credited half its target and
      // scores 0.5 instead of 1.0, and the sample-size penalty halves the total again on top. Paying all three in full
      // would hand 0.65 of a quality score to a candidate that was never really tested.
      val stats = pairs.map(pair => statsFor(pair, List.fill(30)(BigDecimal(10)), spacing = 12.days))

      ScoringFunction.Robust().score(stats) mustBe 0.215 +- 0.0001
    }

    "penalise rather than reject candidates with too few closed trades" in {
      // Both ran for five months. One traded at the rate asked of it and the other at a third of it.
      val scoring = ScoringFunction.Robust()
      val few     = pairs.map(pair => statsFor(pair, List.fill(5)(BigDecimal(10)), spacing = 30.days))
      val enough  = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))

      scoring.score(few) must be > 0.0
      scoring.score(few) must be < scoring.score(enough)
    }

    "increase fitness steadily as a candidate approaches the minimum trade count" in {
      // A window fixes the run at twelve months however few trades land in it, so the floor stays at 120 across the
      // whole sweep and the only thing moving is how close each candidate gets to it. The rate is pinned rather than
      // defaulted because one dataset at the default of two would put a floor of 24 below every count in the sweep.
      val scoring = ScoringFunction.Robust(ScoringFunction.Robust.Config(minTradesPerMonth = 10))
      val scores  = List(30, 60, 90, 120).map { count =>
        scoring.score(List(statsFor(pairs.head, List.fill(count)(BigDecimal(10)), spacing = 3.days, dataWindow = fullYear)))
      }

      // The whole point of ramping instead of gating: every one of these used to score exactly 0.0, leaving
      // selection with nothing to rank.
      scores.foreach(_ must be > 0.0)
      scores mustBe scores.sorted
      scores.distinct must have size scores.size
    }

    "reject candidates with non-positive expectancy" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(10), BigDecimal(-20))))

      ScoringFunction.Robust(permissiveConfig).score(stats) mustBe 0.0
    }

    "reject candidates that generated invalid orders" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(100)), invalidOrderCount = 1))

      ScoringFunction.Robust(permissiveConfig).score(stats) mustBe 0.0
    }

    "penalise candidates below the minimum profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(8), BigDecimal(-7))))

      val met        = ScoringFunction.Robust(permissiveConfig.copy(minProfitFactor = 1.1)).score(stats)
      val fallsShort = ScoringFunction.Robust(permissiveConfig.copy(minProfitFactor = 2.0)).score(stats)

      fallsShort must be > 0.0
      fallsShort must be < met
    }

    "treat a candidate with wins and no losses as having an acceptable profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal("0.001"))))

      ScoringFunction.Robust(permissiveConfig).score(stats) must be > 0.0
    }

    "penalise candidates above the maximum drawdown" in {
      val stats  = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-200), BigDecimal(200)), initialBalance = BigDecimal(1000)))
      val within = ScoringFunction.Robust(permissiveConfig).score(stats)
      val over   = ScoringFunction.Robust(permissiveConfig.copy(maxDrawdownPercent = 15.0)).score(stats)

      over must be > 0.0
      over must be < within
    }

    "penalise candidates whose profit is concentrated in too few datasets" in {
      val stats = List(
        statsFor(pairs(0), List(BigDecimal(100))),
        statsFor(pairs(1), List(BigDecimal(-10))),
        statsFor(pairs(2), List(BigDecimal(-10)))
      )
      val met       = ScoringFunction.Robust(permissiveConfig.copy(minProfitableDatasetRatio = 1.0 / 3.0)).score(stats)
      val tooNarrow = ScoringFunction.Robust(permissiveConfig.copy(minProfitableDatasetRatio = 2.0 / 3.0)).score(stats)

      tooNarrow must be > 0.0
      tooNarrow must be < met
    }

    "penalise candidates whose costs consume too much pre-cost profit" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(50)), costPerTrade = BigDecimal(50)))

      val affordable = ScoringFunction.Robust(permissiveConfig.copy(maxCostToPreCostProfitRatio = 0.6)).score(stats)
      val expensive  = ScoringFunction.Robust(permissiveConfig.copy(maxCostToPreCostProfitRatio = 0.4)).score(stats)

      expensive must be > 0.0
      expensive must be < affordable
    }

    "favor stronger return and recovery when candidates are otherwise alike" in {
      val weaker   = List(statsFor(pairs.head, List(BigDecimal(20), BigDecimal(-10), BigDecimal(20))))
      val stronger = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-10), BigDecimal(100))))
      val scoring  = ScoringFunction.Robust(permissiveConfig)

      scoring.score(stronger) must be > scoring.score(weaker)
    }

    "allow exceptional candidates to exceed a fitness of one" in {
      val stats = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(10000), months = 5))

      ScoringFunction.Robust().score(stats) must be > 1.0
    }

    "bound a runaway metric so a single axis cannot dominate fitness" in {
      val strong  = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(10000), months = 5))
      val extreme = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(10000000), months = 5))
      val scoring = ScoringFunction.Robust()

      // A 1000x jump on the net-return axis buys about 0.03 of fitness, because the component is already deep into
      // saturation. Without a bound the unbounded logarithm would let one lucky axis outweigh every other combined.
      scoring.score(extreme) - scoring.score(strong) must be < 0.03
      // The other three components are undefined here, so each scores exactly its target and the most this candidate
      // can reach is 0.35 * maxComponentScore + 0.65. The asymptote means it stays strictly underneath.
      scoring.score(extreme) must be < 1.7
    }

    "keep ranking candidates a hard ceiling would have scored identically" in {
      val good   = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(2000), months = 5))
      val better = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(4000), months = 5))
      val best   = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(8000), months = 5))

      // Net returns of 1.0, 2.0 and 4.0 against a 0.1 target are all past the point where a ceiling of
      // maxComponentScore used to flatten the component, which handed all three the same fitness of 1.7 and left
      // selection unable to prefer any of them. Saturating asymptotically keeps the ordering intact.
      val scores = List(good, better, best).map(ScoringFunction.Robust().score)

      scores mustBe scores.sorted
      scores.distinct must have size scores.size
    }
  }

  "ScoringFunction.Consistent" should {
    // One a month, which the fixtures below meet by trading once a month, so the sample-size and period-count ramps
    // stay out of the way and whichever consistency constraint a case is about is the only thing moving the score.
    // Cases that widen the window past the months traded have to trade denser than this to keep that true, since the
    // added months raise the floor.
    val permissive = ScoringFunction.Consistent.Config(
      minTradesPerMonth = 1,
      minMonthsCovered = 1,
      minProfitableDatasetRatio = 1.0,
      maxDrawdownPercent = 100.0
    )

    "return zero for no datasets" in {
      ScoringFunction.Consistent().score(Nil) mustBe 0.0
    }

    "assign positive fitness to a candidate that earns steadily" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))

      ScoringFunction.Consistent().score(stats) must be > 0.0
    }

    "scale the trade floor with the length of the run rather than demanding a fixed total" in {
      // Both candidates traded at exactly one a month, so both fall equally short of a rate of two and neither is
      // preferred for having been handed a longer window. A fixed total would have demanded as much of the six-month
      // run as of the twelve-month one, which is the frequency requirement silently doubling whenever the data is
      // halved — and halving the data is exactly what splitting a year into a training and a validation half does.
      val scoring      = ScoringFunction.Consistent(permissive.copy(minTradesPerMonth = 2))
      val sixMonths    = List(statsFor(pairs.head, List.fill(6)(BigDecimal(40))))
      val twelveMonths = List(statsFor(pairs.head, List.fill(12)(BigDecimal(40))))

      def tradeFloor(stats: List[OrderStats]): Option[String] =
        scoring.violations(stats).find(_.constraint == "closed trades").map(_.required)

      tradeFloor(sixMonths) mustBe Some(">= 12 (2 per pair-month over 6 months x 1 pairs)")
      tradeFloor(twelveMonths) mustBe Some(">= 24 (2 per pair-month over 12 months x 1 pairs)")
    }

    "scale the trade floor with the number of datasets rather than demanding a fixed pooled total" in {
      // The count the floor is compared against is pooled over every dataset, so a floor that ignores how many there
      // are asks exactly as much of three pairs as of one. Both candidates here trade at the same rate per pair and
      // fall equally short of it; without the dataset term the three-pair run would clear a floor built for one and
      // the only sample-size guard there is would weaken by the factor the corpus was widened by — at the one moment
      // it was widened in order to strengthen it.
      val scoring    = ScoringFunction.Consistent(permissive.copy(minTradesPerMonth = 2))
      val onePair    = List(statsFor(pairs.head, List.fill(6)(BigDecimal(40))))
      val threePairs = pairs.map(pair => statsFor(pair, List.fill(6)(BigDecimal(40))))

      def tradeFloor(stats: List[OrderStats]): Option[String] =
        scoring.violations(stats).find(_.constraint == "closed trades").map(_.required)

      tradeFloor(onePair) mustBe Some(">= 12 (2 per pair-month over 6 months x 1 pairs)")
      tradeFloor(threePairs) mustBe Some(">= 36 (2 per pair-month over 6 months x 3 pairs)")
    }

    "reject a candidate whose one profitable period pays for several losing ones" in {
      // Eleven months losing 10 each and a twelfth making 500: net profit of 390 across the year, a profit factor
      // above 4, and every pooled figure Robust reads looks healthy. Judged period by period it is one win against
      // eleven losses, which is the shape this scoring function exists to refuse.
      val compensated = List(statsFor(pairs.head, List.fill(11)(BigDecimal(-10)) :+ BigDecimal(500)))
      val steady      = List(statsFor(pairs.head, List.fill(12)(BigDecimal(30))))

      ScoringFunction.Robust(permissiveConfig).score(compensated) must be > 0.0
      ScoringFunction.Consistent(permissive).score(compensated) mustBe 0.0
      ScoringFunction.Consistent(permissive).score(steady) must be > 0.0
    }

    "prefer the steadier of two candidates that earned the same total" in {
      // Both net 240 over twelve months. The lumpy one makes it in three months and gives some back in nine.
      val steady = List(statsFor(pairs.head, List.fill(12)(BigDecimal(20))))
      val lumpy  = List(statsFor(pairs.head, List.fill(9)(BigDecimal(-20)) ::: List.fill(3)(BigDecimal(140))))

      val scoring = ScoringFunction.Consistent(permissive)
      scoring.score(steady) must be > scoring.score(lumpy)
    }

    "disqualify a candidate whose typical period loses money" in {
      // Seven losing months against five winners that more than pay for them: the total is positive but the median
      // period is not, so there is no edge here to have found.
      val stats = List(statsFor(pairs.head, List.fill(7)(BigDecimal(-10)) ::: List.fill(5)(BigDecimal(50))))

      ScoringFunction.Consistent(permissive).score(stats) mustBe 0.0
      ScoringFunction.Consistent(permissive).violations(stats).map(_.constraint) must contain("median period profit")
    }

    "penalise profit concentrated in a single period" in {
      // Nine profitable months out of twelve either way, so the profitable-period ratio and the median are identical;
      // the only difference is how much of the winnings the best month accounts for.
      val spread       = List(statsFor(pairs.head, List.fill(3)(BigDecimal(-5)) ::: List.fill(9)(BigDecimal(40))))
      val concentrated = List(statsFor(pairs.head, List.fill(3)(BigDecimal(-5)) ::: (List.fill(8)(BigDecimal(5)) :+ BigDecimal(320))))
      val scoring      = ScoringFunction.Consistent(permissive)

      scoring.score(concentrated) must be > 0.0
      scoring.score(concentrated) must be < scoring.score(spread)
      scoring.violations(concentrated).map(_.constraint) must contain("most concentrated pair's best month")
    }

    "count the periods a candidate traded through, not the ones it traded in" in {
      // Identical trades and identical profit. One spreads three wins across three consecutive months, the other
      // across a year — so the second sat out nine months that the breakdown has to carry as zeros, or a candidate
      // that trades in a burst reads as perfectly consistent.
      val scoring    = ScoringFunction.Consistent(permissive)
      val continuous = List(statsFor(pairs.head, List.fill(3)(BigDecimal(30))))
      val sporadic   = List(statsFor(pairs.head, List.fill(3)(BigDecimal(30)), spacing = 160.days))

      scoring.score(sporadic) must be < scoring.score(continuous)
      scoring.violations(sporadic).map(_.constraint) must contain("profitable pair-months")
    }

    "count the months at either end that the data covered but the candidate sat out" in {
      // Six winning months and nothing else either way. The first was given exactly those six months; the second was
      // given a full year and opened nothing in the six months at the ends of it. Reading the months off the trades
      // makes the two indistinguishable, and avoiding an unfavourable stretch of a fixed sample is one of the cheapest
      // things for a search to fit.
      val scoring    = ScoringFunction.Consistent(permissive)
      val sixMonths  = List(statsFor(pairs.head, List.fill(6)(BigDecimal(40))))
      val satOutEnds = List(statsFor(pairs.head, List.fill(6)(BigDecimal(40)), dataWindow = fullYear))

      scoring.violations(sixMonths).map(_.constraint) must not contain "profitable pair-months"
      scoring.violations(satOutEnds).map(_.constraint) must contain("profitable pair-months")
      scoring.score(satOutEnds) must be < scoring.score(sixMonths)
    }

    "measure downside across the months the data covered, not only the months it traded" in {
      // A losing month and seven winners, and a window that adds four flat ones. There has to be a losing month for
      // there to be any downside to measure — with none, the ratio is credited rather than computed and four more
      // zeros change nothing about it.
      //
      // Everything else is held equal: the pooled figures are identical, the median is too because the four zeros land
      // below it, the concentration share and pair-month profit factor are unchanged, and the profitable-month ratio is
      // configured out of the way. So the only axis left to separate these is the risk-adjusted one. OrderStats
      // .sortinoRatio cannot separate them: profitByMonth has no key for a month nothing closed in, so it measures
      // dispersion across the eight traded months either way, and disagrees with every constraint on this object about
      // what the run was.
      //
      // Two trades a month rather than one, so that the four months the window adds cannot take the sample-size ramp
      // down with them: at one a month the ramp would fall in the same direction as the axis under test and the
      // assertion would hold without the Sortino difference contributing anything.
      val trades    = List.fill(2)(BigDecimal(-10)) ::: List.fill(14)(BigDecimal(40))
      val scoring   = ScoringFunction.Consistent(permissive.copy(minProfitablePeriodRatio = 0.01))
      val eightBusy = List(statsFor(pairs.head, trades, spacing = 16.days))
      val fourIdle  = List(statsFor(pairs.head, trades, spacing = 16.days, dataWindow = fullYear))

      scoring.score(fourIdle) must be < scoring.score(eightBusy)
    }

    "count a position liquidated after the data ran out in the month it was liquidated" in {
      // A position still open at the end is closed at a mark stamped one interval past the final bar, so a run whose
      // last bar is the last hour of a month realises that profit in the next one — which is what every hourly dataset
      // ending at 23:00 on the last of the month does. Bounding the months by the window alone drops that trade from the
      // evidence while totalProfit keeps counting it, so the two disagree about what the run earned.
      val window  = Some(DataWindow(start, Instant.parse("2025-03-31T23:00:00Z")))
      val scoring = ScoringFunction.Consistent(permissive.copy(minProfitablePeriodRatio = 0.9))
      val winners = List(
        BigDecimal(40) -> Instant.parse("2025-01-15T00:00:00Z"),
        BigDecimal(40) -> Instant.parse("2025-02-15T00:00:00Z"),
        BigDecimal(40) -> Instant.parse("2025-03-15T00:00:00Z")
      )
      val closedInside  = List(statsClosingAt(pairs.head, winners, window))
      val liquidatedOut =
        List(statsClosingAt(pairs.head, winners :+ (BigDecimal(-30) -> Instant.parse("2025-04-01T00:01:40Z")), window))

      scoring.violations(closedInside).map(_.constraint) must not contain "profitable pair-months"
      scoring.violations(liquidatedOut).map(_.constraint) must contain("profitable pair-months")
    }

    "merge a part-period remainder into the last full period rather than scoring it as a period" in {
      // Thirteen months at three months to the period leaves one month over. Standing alone that month is a period
      // holding a third of the profit a period is measured against, and here it is also the only losing month: as its
      // own period it makes one period in five a loser, while merged into the period before it leaves every period
      // profitable, which is what a single bad month inside a good quarter actually is. Coverage is the other half of
      // it — five periods of three months would be reported as fifteen months of data that do not exist.
      val scoring  = ScoringFunction.Consistent(permissive.copy(periodMonths = 3, minMonthsCovered = 20, minProfitablePeriodRatio = 0.9))
      val thirteen = List(statsFor(pairs.head, List.fill(12)(BigDecimal(40)) :+ BigDecimal(-100)))

      scoring.violations(thirteen).map(_.constraint) must not contain "profitable pair-months"
      scoring.violations(thirteen).find(_.constraint == "months covered").map(_.actual) mustBe Some("13 months")
    }

    "not let one pair's good month pay for another pair's bad month" in {
      // Pooled, every month of the year made 10 and the record is spotless. Pair by pair, half of the twenty-four
      // pair-months lost money: one pair earned steadily and the other bled steadily, and pooling before splitting into
      // months is what hides it. The dataset constraint does not catch this, because it only asks whether each pair's
      // year was profitable overall, which is a much weaker question.
      val scoring    = ScoringFunction.Consistent(permissive.copy(minProfitableDatasetRatio = 0.5))
      val offsetting = List(
        statsFor(pairs(0), List.fill(12)(BigDecimal(50))),
        statsFor(pairs(1), List.fill(12)(BigDecimal(-40)))
      )
      val bothSteady = List(
        statsFor(pairs(0), List.fill(12)(BigDecimal(50))),
        statsFor(pairs(1), List.fill(12)(BigDecimal(10)))
      )

      scoring.score(offsetting) must be < scoring.score(bothSteady)
      scoring.violations(offsetting).map(_.constraint) must contain("profitable pair-months")
      scoring.violations(bothSteady).map(_.constraint) must not contain "profitable pair-months"
    }

    "penalise winning periods that only just outweigh the losing ones" in {
      val scoring = ScoringFunction.Consistent(permissive)
      val narrow  = List(statsFor(pairs.head, List.fill(5)(BigDecimal(-20)) ::: List.fill(7)(BigDecimal(16))))
      val wide    = List(statsFor(pairs.head, List.fill(5)(BigDecimal(-20)) ::: List.fill(7)(BigDecimal(60))))

      scoring.score(narrow) must be > 0.0
      scoring.score(narrow) must be < scoring.score(wide)
      scoring.violations(narrow).map(_.constraint) must contain("pair-month profit factor")
    }

    "still apply every pooled constraint Robust applies" in {
      val scoring = ScoringFunction.Consistent(permissive.copy(maxDrawdownPercent = 15.0))
      val stats   = List(
        statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-300), BigDecimal(400)), initialBalance = BigDecimal(1000))
      )

      scoring.violations(stats).map(_.constraint) must contain("max drawdown")
    }

    "report an empty result set" in {
      ScoringFunction.Consistent().violations(Nil).map(_.constraint) mustBe List("dataset count")
    }
  }

  "ScoringFunction.violations" should {
    "report nothing when every constraint is satisfied" in {
      val stats = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))

      ScoringFunction.Robust().violations(stats) mustBe empty
    }

    "report a drawdown breach that scoring merely discounted" in {
      val stats = List(
        statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-300), BigDecimal(400)), initialBalance = BigDecimal(1000))
      )

      // A 27% drawdown against a 15% limit costs this candidate most of its fitness but does not disqualify it, so it
      // still wins a round that turns up nothing better. Ramping is what gives selection a gradient to climb; deciding
      // whether the winner is fit to use is a different question, and only the explicit check answers it.
      ScoringFunction.Robust().score(stats) must be > 0.0
      ScoringFunction.Robust().violations(stats).map(_.constraint) must contain("max drawdown")
    }

    "report a sample too small to trust" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(10)(BigDecimal(10))))

      ScoringFunction.Robust().violations(stats).map(_.constraint) must contain("closed trades")
    }

    "report an empty result set" in {
      ScoringFunction.Robust().violations(Nil).map(_.constraint) mustBe List("dataset count")
    }
  }
}
