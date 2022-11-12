package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
import currexx.backtest.services.TestServices
import syntax.*
import currexx.core.trade.TradeStrategy
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage, ValueSource, ValueTransformation}
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
//      Indicator.TrendChangeDetection(
//        source = ValueSource.Close,
//        transformation = ValueTransformation.sequenced(
//          ValueTransformation.JMA(21,100,7),
//          ValueTransformation.RSX(3)
//        )
//      )
//      Indicator.ThresholdCrossing(
//        source = ValueSource.Close,
//        transformation = ValueTransformation.RSX(3),
//        upperBoundary = 52.0,
//        lowerBoundary = 37.0
//      )
      Indicator.LinesCrossing(
        source = ValueSource.Close,
        line1Transformation = ValueTransformation.HMA(44),
        line2Transformation = ValueTransformation.HMA(43)
      ),
      Indicator.TrendChangeDetection(
        source = ValueSource.Close,
        transformation = ValueTransformation.sequenced(
          ValueTransformation.HMA(4),
//          ValueTransformation.RSX(35)
        )
      ),
//      Indicator.ThresholdCrossing(
//        source = ValueSource.Close,
//        transformation = ValueTransformation.DoubleOutput.STOCH(20, 3, 3),
//        upperBoundary = 75,
//        lowerBoundary = 25
//      )
    )
  )

  override val run: IO[Unit] =
    Stream
      .emits(MarketDataProvider.euusDataset)
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
      .flatMap(stats => logger.info(s"total profit: ${stats.map(_.totalProfit).sum}, median: ${stats.map(_.totalProfit).median}"))
}
