package currexx.backtest.optimizer

import currexx.backtest.{CompletedTrade, DataWindow, MarketMark, OrderStats, OrderStatsCollector, RiskRatio, RiskSettings, TransactionCosts}
import currexx.backtest.types.given
import currexx.clients.broker.BrokerParameters
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.TradeOrder.{Enter, Exit, Position}
import currexx.domain.market.{Currency, CurrencyPair}
import currexx.domain.user.UserId
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

  /** Identical orders earning 300 at the end of March, with the supplied equity path while the position remains open. */
  private def statsMarkedAt(prices: List[(String, String)]): OrderStats = {
    val end          = Instant.parse("2025-03-31T23:00:00Z")
    val userId       = UserId("user-1")
    val brokerParams = BrokerParameters.Oanda("key", true, "account")
    val orders       = List(
      TradeOrderPlacement(userId, Enter(Position.Buy, pairs.head, BigDecimal(1), BigDecimal(1)), brokerParams, start),
      TradeOrderPlacement(userId, Exit(pairs.head, BigDecimal("1.03")), brokerParams, end)
    )
    OrderStatsCollector
      .collect(
        orders = orders,
        settings = RiskSettings(
          unitsPerLot = BigDecimal(10000),
          transactionCosts = TransactionCosts(spreadPips = BigDecimal(0), slippagePipsPerSide = BigDecimal(0))
        ),
        dataWindow = Some(DataWindow(start, end)),
        marketMarks = prices.map { case (time, price) => MarketMark(BigDecimal(price), Instant.parse(time)) }
      )
      .fold(error => fail("Marked trade fixture was rejected", error), identity)
  }

  private val permissiveConfig = ScoringFunction.Robust.Config(
    minTradesPerMonth = 1,
    minProfitableDatasetRatio = 1.0,
    maxDrawdownPercent = 100.0
  )

  private def fourMonthStats(lastMonth: BigDecimal): OrderStats =
    statsClosingAt(
      pairs.head,
      List(BigDecimal(100), BigDecimal(100), BigDecimal(100), lastMonth).zipWithIndex.flatMap { case (profit, month) =>
        (1 to 5).map(day => (profit / 5) -> Instant.parse(f"2025-${month + 1}%02d-${day * 5}%02dT12:00:00Z"))
      },
      Some(DataWindow(start, Instant.parse("2025-04-30T23:00:00Z")))
    )

  List(
    ("Robust", ScoringFunction.Robust(), ScoringFunction.Robust(ScoringFunction.Robust.Config(minTradesPerMonth = 10))),
    ("Consistent", ScoringFunction.Consistent(), ScoringFunction.Consistent(ScoringFunction.Consistent.Config(minTradesPerMonth = 10)))
  ).foreach { case (name, fullConfidence, halfConfidence) =>
    s"ScoringFunction.$name ratio boundaries" should
      List("full" -> fullConfidence, "half" -> halfConfidence).foreach { case (confidence, scoring) =>
        s"approach zero downside continuously at $confidence sample confidence without rewarding a tiny loss" in {
          val zero       = fourMonthStats(BigDecimal(0))
          val gain       = fourMonthStats(BigDecimal("0.01"))
          val losses     = List("-0.01", "-0.0001", "-0.000001").map(value => fourMonthStats(BigDecimal(value)))
          val zeroScore  = scoring.score(List(zero))
          val gainScore  = scoring.score(List(gain))
          val lossScores = losses.map(stats => scoring.score(List(stats)))

          zero.sortinoRatio mustBe RiskRatio.ZeroDeviation
          losses.foreach(_.sortinoRatio mustBe a[RiskRatio.Defined])
          losses.head.totalProfit must be < gain.totalProfit
          (lossScores :+ zeroScore :+ gainScore).foreach { score =>
            score.isFinite mustBe true
            score must be > 0.0
            score must be <= 3.0
          }
          lossScores mustBe lossScores.sorted
          lossScores.foreach(_ must be < zeroScore)
          gainScore must be >= zeroScore
          (zeroScore - lossScores.last) must be < 0.000001
          // Changing the sign of this tiny month must not change acceptance or introduce another penalty.
          losses.foreach(stats => scoring.violations(List(stats)) mustBe scoring.violations(List(gain)))
        }
      }
  }

  "ScoringFunction.Robust" should {
    "return zero for no datasets" in {
      ScoringFunction.Robust().score(Nil) mustBe 0.0
    }

    "credit zero-downside metrics with the limit of their bounded score" in {
      val stats = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))

      // Never a drawdown, never a losing month and never a losing trade, so recovery factor, Sortino and
      // expectancy-to-loss are all undefined and each reaches its bounded maximum. That leaves net return (150
      // trades of 10 against a pooled balance of 30000, so half of the 0.1 target) as the only component below
      // full marks: 0.35 * 0.5 + 3 * (0.30 + 0.175 + 0.175).
      ScoringFunction.Robust().score(stats) mustBe 2.125 +- 0.0001
    }

    "withhold credit for a Sortino ratio that could never be measured" in {
      val scoring = ScoringFunction.Robust()
      val spread  = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(100), months = 5))
      val burst   = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10)), spacing = 1.hour))

      // Identical trades, profit and drawdown; the only difference is that the burst closed all 150 of them inside one
      // calendar month, leaving no monthly series to measure downside across. Paying that as generously as a year
      // without a losing month buys evidence that was never produced. Trading rarely is already covered by the
      // sample-size penalty, but trading in a burst is not, so the two Nones cannot share a fallback.
      scoring.score(burst) mustBe scoring.score(spread) - 0.525 +- 0.0001
    }

    "discount the credit for an undefined metric when the sample behind it is thin" in {
      // Spread thin rather than simply few: 90 closed trades across twelve months is half the 180 that rate asks of
      // three pairs over a run that long, so each ratio scores 1.5 instead of 3.0, and the sample-size penalty halves
      // the total again on top. The same confidence discount applies to measured ratios with a nonzero denominator.
      val stats = pairs.map(pair => statsFor(pair, List.fill(30)(BigDecimal(10)), spacing = 12.days))

      ScoringFunction.Robust().score(stats) mustBe 0.54 +- 0.0001
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

    "penalise unrealized drawdown despite identical winning trades and monthly returns" in {
      val monthEnds = List(
        "2025-01-31T23:00:00Z" -> "1.01",
        "2025-02-28T23:00:00Z" -> "1.02",
        "2025-03-31T23:00:00Z" -> "1.03"
      )
      val steady  = statsMarkedAt(("2025-01-15T00:00:00Z" -> "1.00") :: monthEnds)
      val exposed = statsMarkedAt(("2025-01-15T00:00:00Z" -> "0.80") :: monthEnds)
      val scoring = ScoringFunction.Robust(permissiveConfig.copy(maxDrawdownPercent = 15.0))

      exposed.completedTrades mustBe steady.completedTrades
      exposed.profitByMonth mustBe steady.profitByMonth
      exposed.maxDrawdownPercent mustBe BigDecimal(20)
      steady.maxDrawdownPercent mustBe BigDecimal(0)
      scoring.score(List(exposed)) must be < scoring.score(List(steady))
      scoring.violations(List(exposed)).map(_.constraint) must contain("max drawdown")
      scoring.violations(List(steady)).map(_.constraint) must not contain "max drawdown"
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
      // The other three components have no downside and reach 3.0. The return asymptote keeps the total below 3.0.
      scoring.score(extreme) must be > 2.9
      scoring.score(extreme) must be < 3.0
    }

    "keep ranking candidates a hard ceiling would have scored identically" in {
      val good   = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(2000), months = 5))
      val better = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(4000), months = 5))
      val best   = pairs.map(pair => statsAtFloorRate(pair, BigDecimal(8000), months = 5))

      // Net returns of 1.0, 2.0 and 4.0 against a 0.1 target are all past the point where a ceiling of
      // maxComponentScore used to flatten the component, which handed all three the same fitness and left
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

    "withhold Sortino credit when only one period was measured" in {
      val stats = statsFor(pairs.head, List.fill(10)(BigDecimal(10)), spacing = 1.hour)

      stats.sortinoRatio mustBe RiskRatio.InsufficientData
      // Median return contributes 0.354545; period profit factor, recovery and edge contribute 0.6 + 0.6 + 0.45.
      // The unmeasurable Sortino contributes nothing, unlike a measured series without losing periods.
      ScoringFunction.Consistent(permissive).score(List(stats)) mustBe 2.0045454545 +- 0.000000001
    }

    // Every fixture trades once per pair-month, below the configured rate of two. Changing the run length and
    // dataset count independently protects both factors in the pooled floor against becoming a fixed total.
    List(
      ("require twelve trades over six months for one pair", 6, 1, ">= 12 (2 per pair-month over 6 months x 1 pairs)"),
      ("double the trade floor when the run grows to twelve months", 12, 1, ">= 24 (2 per pair-month over 12 months x 1 pairs)"),
      ("triple the trade floor when the corpus grows to three pairs", 6, 3, ">= 36 (2 per pair-month over 6 months x 3 pairs)")
    ).foreach { case (description, months, pairCount, expected) =>
      description in {
        val scoring = ScoringFunction.Consistent(permissive.copy(minTradesPerMonth = 2))
        val stats   = pairs.take(pairCount).map(pair => statsFor(pair, List.fill(months)(BigDecimal(40))))

        scoring.violations(stats).find(_.constraint == "closed trades").map(_.required) mustBe Some(expected)
      }
    }

    "reject a candidate whose one profitable period pays for several losing ones" in {
      // Eleven months losing 10 each and a twelfth making 500: net profit of 390 across the year, a profit factor
      // above 4, and every pooled figure Robust reads looks healthy. Judged period by period it is one win against
      // eleven losses, which is the shape this scoring function exists to refuse. The median period is what refuses it —
      // concentration only discounts, so a single winning month cannot be disqualifying on its own.
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

    "rank calendar equity gains before a trade closes rather than attributing all profit to its closing month" in {
      val steady = statsMarkedAt(
        List(
          "2025-01-31T23:00:00Z" -> "1.01",
          "2025-02-28T23:00:00Z" -> "1.02",
          "2025-03-31T23:00:00Z" -> "1.03"
        )
      )
      val stalled = statsMarkedAt(
        List(
          "2025-01-31T23:00:00Z" -> "1.01",
          "2025-02-28T23:00:00Z" -> "1.01",
          "2025-03-31T23:00:00Z" -> "1.03"
        )
      )
      val scoring = ScoringFunction.Consistent(permissive.copy(minProfitablePeriodRatio = 0.9))

      stalled.completedTrades mustBe steady.completedTrades
      stalled.totalProfit mustBe steady.totalProfit
      stalled.maxDrawdown mustBe steady.maxDrawdown
      steady.profitByMonth mustBe Map("2025-01" -> BigDecimal(100), "2025-02" -> BigDecimal(100), "2025-03" -> BigDecimal(100))
      stalled.profitByMonth mustBe Map("2025-01" -> BigDecimal(100), "2025-02" -> BigDecimal(0), "2025-03" -> BigDecimal(200))
      scoring.score(List(steady)) must be > scoring.score(List(stalled))
      scoring.violations(List(steady)).map(_.constraint) must not contain "profitable pair-months"
      scoring.violations(List(stalled)).map(_.constraint) must contain("profitable pair-months")
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
      // the only difference is how much of the winnings the best month accounts for. It discounts and never disqualifies:
      // total concentration costs half the score, not all of it.
      val spread       = List(statsFor(pairs.head, List.fill(3)(BigDecimal(-5)) ::: List.fill(9)(BigDecimal(40))))
      val concentrated = List(statsFor(pairs.head, List.fill(3)(BigDecimal(-5)) ::: (List.fill(8)(BigDecimal(5)) :+ BigDecimal(320))))
      val scoring      = ScoringFunction.Consistent(permissive)

      scoring.score(concentrated) must be > 0.0
      scoring.score(concentrated) must be < scoring.score(spread)
      scoring.violations(concentrated).map(_.constraint) must contain("most concentrated pair's best month")
    }

    "discount rather than annihilate a candidate carried by one period in one pair" in {
      // The shape that made every promoted strategy score exactly zero. A pair with one winning period has a share of
      // 1.0 and no way to do better, so zeroing there wiped the whole fold on the strength of one pair out of six — and
      // through the product over folds, the whole run.
      val scoring  = ScoringFunction.Consistent(permissive)
      val steady   = pairs.take(2).map(pair => statsFor(pair, List.fill(6)(BigDecimal(40))))
      val oneMonth =
        statsFor(pairs(2), List(BigDecimal(-1), BigDecimal(-1), BigDecimal(30), BigDecimal(-1), BigDecimal(-1), BigDecimal(-1)))

      scoring.score(steady :+ oneMonth) must be > 0.0
      scoring.violations(steady :+ oneMonth).map(_.constraint) must contain("most concentrated pair's best month")
    }

    "scale the concentration limit to the periods there were" in {
      // The same 0.75 share of the winnings in the best period. Over twelve months that is one month carrying the pair;
      // over three it is barely worse than the 0.333 an even split forces, and a limit calibrated on a year would reject
      // every candidate a three-month fold could produce.
      val scoring      = ScoringFunction.Consistent(permissive)
      val threeMonths  = List(statsFor(pairs.head, List(BigDecimal(10), BigDecimal(30), BigDecimal(-1))))
      val twelveMonths = List(statsFor(pairs.head, List.fill(5)(BigDecimal(-2)) ::: (List.fill(6)(BigDecimal(20)) :+ BigDecimal(360))))

      scoring.violations(threeMonths).map(_.constraint) must not contain "most concentrated pair's best month"
      scoring.violations(twelveMonths).map(_.constraint) must contain("most concentrated pair's best month")
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
      // configured out of the way. So the only axis left to separate these is the risk-adjusted one. Both scoring
      // functions and the reported Sortino must account for the four flat months in the same way.
      //
      // Two trades a month rather than one, so that the four months the window adds cannot take the sample-size ramp
      // down with them: at one a month the ramp would fall in the same direction as the axis under test and the
      // assertion would hold without the Sortino difference contributing anything.
      val trades    = List.fill(2)(BigDecimal(-10)) ::: List.fill(14)(BigDecimal(40))
      val scoring   = ScoringFunction.Consistent(permissive.copy(minProfitablePeriodRatio = 0.01))
      val eightBusy = List(statsFor(pairs.head, trades, spacing = 16.days))
      val fourIdle  = List(statsFor(pairs.head, trades, spacing = 16.days, dataWindow = fullYear))

      eightBusy.head.profitByMonth must have size 8
      fourIdle.head.profitByMonth must have size 12
      fourIdle.head.sortinoRatio.toOption.get mustBe eightBusy.head.sortinoRatio.toOption.get * math.sqrt(8.0 / 12.0) +- 0.0001
      scoring.score(fourIdle) must be < scoring.score(eightBusy)
      ScoringFunction.Robust(permissiveConfig).score(fourIdle) must be < ScoringFunction.Robust(permissiveConfig).score(eightBusy)
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

    "bill the trade floor for the months of data, not the month a final position spilled into" in {
      // The other half of the case above. The profit series has to span April, because that is where the liquidated
      // position realised its loss; the trade floor must not, or three months of data are billed for a fourth month's
      // trades. The second assertion is the one that matters: it pins that "months covered" still reads the union, so the
      // two month counts cannot later be collapsed back into one.
      val window  = Some(DataWindow(start, Instant.parse("2025-03-31T23:00:00Z")))
      val scoring = ScoringFunction.Consistent(permissive.copy(minTradesPerMonth = 10, minMonthsCovered = 20))
      val trades  = List(
        BigDecimal(40)  -> Instant.parse("2025-01-15T00:00:00Z"),
        BigDecimal(40)  -> Instant.parse("2025-02-15T00:00:00Z"),
        BigDecimal(40)  -> Instant.parse("2025-03-15T00:00:00Z"),
        BigDecimal(-30) -> Instant.parse("2025-04-01T00:01:40Z")
      )
      val stats = List(statsClosingAt(pairs.head, trades, window))

      scoring.violations(stats).find(_.constraint == "closed trades").map(_.required) mustBe
        Some(">= 30 (10 per pair-month over 3 months x 1 pairs)")
      scoring.violations(stats).find(_.constraint == "months covered").map(_.actual) mustBe Some("4 months")
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
      // year was profitable overall, which is a much weaker question. The bleeding pair never won a month, so it has no
      // concentration to report — an absence of winnings is not a concentrated one.
      val scoring    = ScoringFunction.Consistent(permissive.copy(minProfitableDatasetRatio = 0.5))
      val offsetting = List(
        statsFor(pairs(0), List.fill(12)(BigDecimal(50))),
        statsFor(pairs(1), List.fill(12)(BigDecimal(-40)))
      )
      val bothSteady = List(
        statsFor(pairs(0), List.fill(12)(BigDecimal(50))),
        statsFor(pairs(1), List.fill(12)(BigDecimal(10)))
      )

      scoring.score(offsetting) must be > 0.0
      scoring.score(offsetting) must be < scoring.score(bothSteady)
      scoring.violations(offsetting).map(_.constraint) must contain("profitable pair-months")
      scoring.violations(offsetting).map(_.constraint) must not contain "most concentrated pair's best month"
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
