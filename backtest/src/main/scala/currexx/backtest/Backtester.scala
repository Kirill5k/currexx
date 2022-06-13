package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
import currexx.backtest.services.{TestMarketService, TestMonitorService, TestServices, TestSignalService, TestTradeService}
import currexx.clients.broker.BrokerParameters
import currexx.core.common.action.{Action, ActionDispatcher, ActionProcessor}
import currexx.core.market.MarketState
import currexx.core.signal.{SignalSettings, TriggerFrequency}
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.market.{CurrencyPair, Indicator, Interval, MovingAverage, ValueSource, ValueTransformation}
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import squants.market.{EUR, GBP}
import fs2.Stream

import scala.concurrent.duration.*

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val settings = TestSettings.make(
    Indicator.TrendChangeDetection(
      source = ValueSource.Close,
      transformation = ValueTransformation.sequenced(
        //          ValueTransfor0mation.NMA(9, 4, 8d, MovingAverage.Weighted),
        //          ValueTransformation.WMA(5),
        ValueTransformation.HMA(5)
        //          ValueTransformation.Kalman(0.85),
      )
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
      .flatMap(stats => logger.info(s"total profit: ${stats.map(_.totalProfit).sum}"))
}
