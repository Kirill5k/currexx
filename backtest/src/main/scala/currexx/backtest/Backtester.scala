package currexx.backtest

import cats.effect.{IO, IOApp}
import cats.syntax.flatMap.*
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
          ValueTransformation.NMA(9, 3, 8d, MovingAverage.Weighted)
        )
      )
    )
  )

  val tradingParameters = TradingParameters(BigDecimal(0.1), None, None, None)
  val tradeSettings     = TradeSettings(userId, TradeStrategy.TrendChange, BrokerParameters.Vindaloo("1"), tradingParameters)

  override val run: IO[Unit] =
    for
      dispatcher <- ActionDispatcher.make[IO]
      market     <- TestMarketService.make[IO](initialMarketState, dispatcher)
      monitor    <- TestMonitorService.make[IO]
      trade      <- TestTradeService.make[IO](tradeSettings, dispatcher)
      signal     <- TestSignalService.make[IO](signalSettings, dispatcher)
      processor  <- ActionProcessor.make[IO](dispatcher, monitor, signal, market, trade)
      _ <- MarketDataProvider
        .read[IO](currencyPair, Interval.D1, "eur-gbp-1d-2021-2022.csv")
        .flatMap { data =>
          Stream.eval(dispatcher.dispatch(Action.ProcessMarketData(userId, data))) >>
            Stream.eval(dispatcher.numberOfPendingActions).delayBy(10.milli).repeat.takeWhile(_ > 0)
        }
        .concurrently(processor.run)
        .compile
        .drain
      placedOrders <- trade.getAllOrders(userId)
      _            <- logger.info(s"placed orders stats ${OrderStatsCollector.collect(placedOrders)}")
    yield ()
}
