package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.clients.broker.BrokerParameters
import currexx.core.market.MarketState
import currexx.core.signal.SignalSettings
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.market.{CurrencyPair, IndicatorParameters, Interval}
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import squants.market.{EUR, GBP}

object Backtester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val userId       = UserId(ObjectId.get)
  val currencyPair = CurrencyPair(EUR, GBP)

  val initialMarketState = MarketState(userId, currencyPair, None, None, Map.empty, None)

  val signalSettings = SignalSettings(
    userId,
    List(
      IndicatorParameters.HMA(16),
      IndicatorParameters.MACD(fastLength = 12, slowLength = 26, signalSmoothing = 9),
      IndicatorParameters.RSI(length = 14, upperLine = 70, lowerLine = 30)
    )
  )

  val tradingParameters = TradingParameters(BigDecimal(0.1), None, None, None)
  val tradeSettings     = TradeSettings(userId, TradeStrategy.HMABasic, BrokerParameters.Vindaloo("1"), tradingParameters)

  override val run: IO[Unit] = ???
}
