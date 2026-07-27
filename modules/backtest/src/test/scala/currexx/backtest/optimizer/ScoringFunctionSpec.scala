package currexx.backtest.optimizer

import currexx.backtest.optimizer.IndicatorEvaluator.ScoringFunction
import currexx.backtest.{CompletedTrade, OrderStats, RiskSettings}
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
      spacing: FiniteDuration = 32.days
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
      invalidOrderCount = invalidOrderCount
    )
  }

  private val permissiveConfig = ScoringFunction.RobustConfig(
    minClosedTrades = 1,
    minProfitableDatasetRatio = 1.0,
    maxDrawdownPercent = 100.0
  )

  "ScoringFunction.robust" should {
    "return zero for no datasets" in {
      ScoringFunction.robust()(Nil) mustBe 0.0
    }

    "assign positive fitness to a robust candidate" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))

      ScoringFunction.robust()(stats) must be > 0.0
    }

    "credit metrics whose denominator is undefined with their target rather than zero" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))

      // Never a drawdown, never a losing month and never a losing trade, so recovery factor, Sortino and
      // expectancy-to-loss are all undefined and each is worth exactly its target. That leaves net return (150
      // trades of 10 against a pooled balance of 30000, so half of the 0.1 target) as the only component below
      // full marks: 0.35 * 0.5 + 0.30 + 0.175 + 0.175.
      ScoringFunction.robust()(stats) mustBe 0.825 +- 0.0001
    }

    "withhold credit for a Sortino ratio that could never be measured" in {
      val scoring = ScoringFunction.robust()
      val spread  = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))
      val burst   = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10)), spacing = 1.hour))

      // Identical trades, profit and drawdown; the only difference is that the burst closed all 150 of them inside one
      // calendar month, leaving no monthly series to measure downside across. Paying that as generously as a year
      // without a losing month buys evidence that was never produced. Trading rarely is already covered by the
      // sample-size penalty, but trading in a burst is not, so the two Nones cannot share a fallback.
      scoring(burst) mustBe scoring(spread) - 0.175 +- 0.0001
    }

    "discount the credit for an undefined metric when the sample behind it is thin" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(25)(BigDecimal(10))))

      // 75 closed trades is half of the 150 the config asks for, so each of the three undefined metrics is credited
      // half its target and scores 0.5 instead of 1.0, and the sample-size penalty halves the total again on top.
      // Paying all three in full would hand 0.65 of a quality score to a candidate that was never really tested.
      ScoringFunction.robust()(stats) mustBe 0.20625 +- 0.0001
    }

    "penalise rather than reject candidates with too few closed trades" in {
      val scoring = ScoringFunction.robust()
      val few     = pairs.map(pair => statsFor(pair, List.fill(10)(BigDecimal(10))))
      val enough  = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))

      scoring(few) must be > 0.0
      scoring(few) must be < scoring(enough)
    }

    "increase fitness steadily as a candidate approaches the minimum trade count" in {
      val scoring = ScoringFunction.robust()
      val scores  = List(30, 60, 90, 120, 150).map(count => scoring(List(statsFor(pairs.head, List.fill(count)(BigDecimal(10))))))

      // The whole point of ramping instead of gating: every one of these used to score exactly 0.0, leaving
      // selection with nothing to rank.
      scores.foreach(_ must be > 0.0)
      scores mustBe scores.sorted
      scores.distinct must have size scores.size
    }

    "reject candidates with non-positive expectancy" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(10), BigDecimal(-20))))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "reject candidates that generated invalid orders" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(100)), invalidOrderCount = 1))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "penalise candidates below the minimum profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(8), BigDecimal(-7))))

      val met        = ScoringFunction.robust(permissiveConfig.copy(minProfitFactor = 1.1))(stats)
      val fallsShort = ScoringFunction.robust(permissiveConfig.copy(minProfitFactor = 2.0))(stats)

      fallsShort must be > 0.0
      fallsShort must be < met
    }

    "treat a candidate with wins and no losses as having an acceptable profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal("0.001"))))

      ScoringFunction.robust(permissiveConfig)(stats) must be > 0.0
    }

    "penalise candidates above the maximum drawdown" in {
      val stats  = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-200), BigDecimal(200)), initialBalance = BigDecimal(1000)))
      val config = permissiveConfig.copy(minClosedTrades = 3)

      val within = ScoringFunction.robust(config)(stats)
      val over   = ScoringFunction.robust(config.copy(maxDrawdownPercent = 15.0))(stats)

      over must be > 0.0
      over must be < within
    }

    "penalise candidates whose profit is concentrated in too few datasets" in {
      val stats = List(
        statsFor(pairs(0), List(BigDecimal(100))),
        statsFor(pairs(1), List(BigDecimal(-10))),
        statsFor(pairs(2), List(BigDecimal(-10)))
      )
      val config = permissiveConfig.copy(minClosedTrades = 3)

      val met       = ScoringFunction.robust(config.copy(minProfitableDatasetRatio = 1.0 / 3.0))(stats)
      val tooNarrow = ScoringFunction.robust(config.copy(minProfitableDatasetRatio = 2.0 / 3.0))(stats)

      tooNarrow must be > 0.0
      tooNarrow must be < met
    }

    "penalise candidates whose costs consume too much pre-cost profit" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(50)), costPerTrade = BigDecimal(50)))

      val affordable = ScoringFunction.robust(permissiveConfig.copy(maxCostToPreCostProfitRatio = 0.6))(stats)
      val expensive  = ScoringFunction.robust(permissiveConfig.copy(maxCostToPreCostProfitRatio = 0.4))(stats)

      expensive must be > 0.0
      expensive must be < affordable
    }

    "favor stronger return and recovery when candidates are otherwise alike" in {
      val weaker   = List(statsFor(pairs.head, List(BigDecimal(20), BigDecimal(-10), BigDecimal(20))))
      val stronger = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-10), BigDecimal(100))))
      val scoring  = ScoringFunction.robust(permissiveConfig.copy(minClosedTrades = 3))

      scoring(stronger) must be > scoring(weaker)
    }

    "allow exceptional candidates to exceed a fitness of one" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(1000))))

      ScoringFunction.robust()(stats) must be > 1.0
    }

    "bound a runaway metric so a single axis cannot dominate fitness" in {
      val strong  = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(1000))))
      val extreme = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(1000000))))
      val scoring = ScoringFunction.robust()

      // A 1000x jump on the net-return axis buys about 0.03 of fitness, because the component is already deep into
      // saturation. Without a bound the unbounded logarithm would let one lucky axis outweigh every other combined.
      scoring(extreme) - scoring(strong) must be < 0.03
      // The other three components are undefined here, so each scores exactly its target and the most this candidate
      // can reach is 0.35 * maxComponentScore + 0.65. The asymptote means it stays strictly underneath.
      scoring(extreme) must be < 1.7
    }

    "keep ranking candidates a hard ceiling would have scored identically" in {
      val good   = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(200))))
      val better = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(400))))
      val best   = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(800))))

      // Net returns of 1.0, 2.0 and 4.0 against a 0.1 target are all past the point where a ceiling of
      // maxComponentScore used to flatten the component, which handed all three the same fitness of 1.7 and left
      // selection unable to prefer any of them. Saturating asymptotically keeps the ordering intact.
      val scores = List(good, better, best).map(ScoringFunction.robust())

      scores mustBe scores.sorted
      scores.distinct must have size scores.size
    }
  }

  "ScoringFunction.violations" should {
    "report nothing when every constraint is satisfied" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(10))))

      ScoringFunction.violations(stats) mustBe empty
    }

    "report a drawdown breach that scoring merely discounted" in {
      val stats = List(
        statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-300), BigDecimal(400)), initialBalance = BigDecimal(1000))
      )

      // A 27% drawdown against a 15% limit costs this candidate most of its fitness but does not disqualify it, so it
      // still wins a round that turns up nothing better. Ramping is what gives selection a gradient to climb; deciding
      // whether the winner is fit to use is a different question, and only the explicit check answers it.
      ScoringFunction.robust()(stats) must be > 0.0
      ScoringFunction.violations(stats).map(_.constraint) must contain("max drawdown")
    }

    "report a sample too small to trust" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(10)(BigDecimal(10))))

      ScoringFunction.violations(stats).map(_.constraint) must contain("closed trades")
    }

    "report an empty result set" in {
      ScoringFunction.violations(Nil).map(_.constraint) mustBe List("dataset count")
    }
  }
}
