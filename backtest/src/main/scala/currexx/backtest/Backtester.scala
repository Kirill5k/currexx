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

  val userId       = UserId(ObjectId.get)
  val currencyPair = CurrencyPair(EUR, GBP)

  val initialMarketState = MarketState(userId, currencyPair, None, None, Map.empty, None)

  val signalSettings = SignalSettings(
    userId,
    TriggerFrequency.OncePerDay,
    List(
      Indicator.TrendChangeDetection(
        source = ValueSource.Close,
        transformation = ValueTransformation.sequenced(
          ValueTransformation.Kalman(0.25),
          ValueTransformation.HMA(6),
//          ValueTransformation.NMA(9, 3, 8d, MovingAverage.Weighted),
        )
      )
    )
  )

  val tradingParameters = TradingParameters(BigDecimal(0.1), None, None, None)
  val tradeSettings     = TradeSettings(userId, TradeStrategy.TrendChangeAggressive, BrokerParameters.Vindaloo("1"), tradingParameters)

  override val run: IO[Unit] =
    Stream
      .emits(List(
        "aud-cad-1d.csv",
        "aud-jpy-1d.csv",
        "cad-jpy-1d.csv",
        "eur-aud-1d.csv",
        "eur-chf-1d.csv",
        "eur-gbp-1d.csv",
        "gbp-jpy-1d.csv",
        "nzd-cad-1d.csv",
        "nzd-chf-1d.csv",
        "nzd-jpy-1d.csv",
        "usd-dkk-1d.csv",
        "usd-jpy-1d.csv",
        "usd-nok-1d.csv",
        "usd-pln-1d.csv",
      ))
      .evalMap { filePath =>
        for
          _        <- logger.info(s"Processing $filePath")
          services <- TestServices.make[IO](initialMarketState, tradeSettings, signalSettings)
          _ <- MarketDataProvider
            .read[IO](currencyPair, Interval.D1, filePath)
            .evalMap(services.processMarketData(userId))
            .compile
            .drain
          placedOrders <- services.getAllOrders(userId)
          _            <- logger.info(s"placed orders stats ${OrderStatsCollector.collect(placedOrders)}")
        yield ()
      }
      .compile
      .drain
}
