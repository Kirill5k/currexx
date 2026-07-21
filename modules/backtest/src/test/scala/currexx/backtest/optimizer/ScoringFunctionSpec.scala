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
      invalidOrderCount: Int = 0
  ): OrderStats = {
    val trades = netProfits.zipWithIndex.map { case (netProfit, index) =>
      val closedAt = start.plusSeconds(index.toLong * 32.days.toSeconds)
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
      openPositions = Nil,
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

    "reject candidates with too few closed trades" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(49)(BigDecimal(10))))

      ScoringFunction.robust()(stats) mustBe 0.0
    }

    "reject candidates with non-positive expectancy" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(10), BigDecimal(-20))))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "reject candidates below the minimum profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(8), BigDecimal(-7))))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "treat a candidate with wins and no losses as having an acceptable profit factor" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal("0.001"))))

      ScoringFunction.robust(permissiveConfig)(stats) must be > 0.0
    }

    "reject candidates above the maximum drawdown" in {
      val stats  = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-200), BigDecimal(200)), initialBalance = BigDecimal(1000)))
      val config = permissiveConfig.copy(minClosedTrades = 3, maxDrawdownPercent = 15.0)

      ScoringFunction.robust(config)(stats) mustBe 0.0
    }

    "reject candidates whose profit is concentrated in too few datasets" in {
      val stats = List(
        statsFor(pairs(0), List(BigDecimal(100))),
        statsFor(pairs(1), List(BigDecimal(-10))),
        statsFor(pairs(2), List(BigDecimal(-10)))
      )
      val config = permissiveConfig.copy(minClosedTrades = 3, minProfitableDatasetRatio = 2.0 / 3.0)

      ScoringFunction.robust(config)(stats) mustBe 0.0
    }

    "reject candidates whose costs consume too much pre-cost profit" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(50)), costPerTrade = BigDecimal(50)))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "reject candidates that generated invalid orders" in {
      val stats = List(statsFor(pairs.head, List(BigDecimal(100)), invalidOrderCount = 1))

      ScoringFunction.robust(permissiveConfig)(stats) mustBe 0.0
    }

    "favor stronger return and recovery when candidates pass the same gates" in {
      val weaker   = List(statsFor(pairs.head, List(BigDecimal(20), BigDecimal(-10), BigDecimal(20))))
      val stronger = List(statsFor(pairs.head, List(BigDecimal(100), BigDecimal(-10), BigDecimal(100))))
      val scoring  = ScoringFunction.robust(permissiveConfig.copy(minClosedTrades = 3))

      scoring(stronger) must be > scoring(weaker)
    }

    "allow exceptional candidates to exceed a fitness of one" in {
      val stats = pairs.map(pair => statsFor(pair, List.fill(50)(BigDecimal(1000))))

      ScoringFunction.robust()(stats) must be > 1.0
    }
  }
}
