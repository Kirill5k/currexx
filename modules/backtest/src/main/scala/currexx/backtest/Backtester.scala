package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.backtest.syntax.*
import currexx.core.trade.TradeStrategy
import currexx.domain.market.Currency.{EUR, GBP}
import currexx.domain.market.ValueTransformation.*
import currexx.domain.market.*
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val settings = TestSettings.make(
    CurrencyPair(EUR, GBP),
    TradeStrategy.KeltnerChannel,
    List(
      Indicator.LinesCrossing(ValueSource.Close,JMA(45,100,3),JMA(9,100,2)),
      Indicator.TrendChangeDetection(ValueSource.Close, sequenced(NMA(5,17,9.5, MovingAverage.Hull))),
      Indicator.ThresholdCrossing(ValueSource.Close, RSX(41),44.0,13.0),
      Indicator.KeltnerChannel(ValueSource.Close, JMA(45,100,3),JMA(9,100,2), 30, 1.5)
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
        logger.info(s"total profit: ${stats.map(_.totalProfit).sum}, median: ${stats.map(_.totalProfit).median}, median loss: ${stats.map(_.meanLoss).median}")
      }
}
