package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
import currexx.backtest.services.TestServices
import syntax.*
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage, ValueSource, ValueTransformation}
import currexx.domain.market.ValueTransformation.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import fs2.Stream
import currexx.domain.market.Currency.{EUR, GBP}

import scala.concurrent.duration.*

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val settings = TestSettings.make(
    CurrencyPair(EUR, GBP),
    TradeStrategy.TrendChangeAggressive,
    List(
      Indicator.LinesCrossing(ValueSource.Close,JMA(21,0,7),JMA(45,100,1)),
      Indicator.TrendChangeDetection(ValueSource.Close, sequenced(NMA(6,21,13.0, MovingAverage.Hull))),
      Indicator.ThresholdCrossing(ValueSource.Close, RSX(41),44.0,13.0)
    )
  )

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.majors)
      .evalMap { filePath =>
        for
          _        <- logger.info(s"Processing $filePath")
          services <- TestServices.make[IO](settings)
          _ <- MarketDataProvider
            .read[IO](filePath, settings.currencyPair)
            .through(services.processMarketData)
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
          _          <- logger.info(orderStats.toString)
        yield orderStats
      }
      .compile
      .toList
      .flatMap { stats =>
        logger.info(s"total profit: ${stats.map(_.totalProfit).sum}, median: ${stats.map(_.totalProfit).median}, mean loss: ${stats.map(_.meanLoss).mean}")
      }
}
