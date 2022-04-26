package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.calculations.{MomentumOscillatorCalculator, MovingAverageCalculator}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorParameters, MarketTimeSeriesData}
import fs2.Stream

import scala.util.Try

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]
  def getSettings(uid: UserId): F[Option[SignalSettings]]
  def updateSettings(settings: SignalSettings): F[Unit]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def getSettings(uid: UserId): F[Option[SignalSettings]] = settingsRepo.get(uid)
  override def updateSettings(settings: SignalSettings): F[Unit]   = settingsRepo.update(settings)
  override def getAll(uid: UserId): F[List[Signal]]                = signalRepo.getAll(uid)
  override def submit(signal: Signal): F[Unit] =
    signalRepo.save(signal) >> dispatcher.dispatch(Action.ProcessSignal(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    Stream
      .eval(getSettings(uid))
      .evalMap {
        case Some(settings) =>
          settings.pure[F]
        case None =>
          val defaultSettings = SignalSettings.default(uid)
          settingsRepo.update(defaultSettings).as(defaultSettings)
      }
      .flatMap { settings =>
        Stream.emits(
          settings.indicators.flatMap {
            case macd: IndicatorParameters.MACD   => SignalService.detectMacdCrossing(uid, data, macd)
            case rsi: IndicatorParameters.RSI     => SignalService.detectRsiCrossing(uid, data, rsi)
            case stoch: IndicatorParameters.STOCH => None
          }
        )
      }
      .evalMap(submit)
      .compile
      .drain
}

object SignalService:

  def detectMacdCrossing(uid: UserId, data: MarketTimeSeriesData, macd: IndicatorParameters.MACD): Option[Signal] = {
    val (macdLine, signalLine) = MovingAverageCalculator.macdWithSignal(
      values = data.prices.map(_.close).toList,
      fastLength = macd.fastLength,
      slowLength = macd.slowLength,
      signalSmoothing = macd.signalSmoothing
    )
    Condition
      .lineCrossing(macdLine.head, signalLine.head, macdLine.drop(1).head, signalLine.drop(1).head)
      .map(c => Signal(uid, data.currencyPair, macd.indicator, c, data.prices.head.time))
  }

  def detectRsiCrossing(uid: UserId, data: MarketTimeSeriesData, rsi: IndicatorParameters.RSI): Option[Signal] = {
    val rsis       = MomentumOscillatorCalculator.rsi(data.prices.map(_.close).toList, rsi.length)
    val currRsi    = rsis.head
    val below      = Option.when(currRsi > rsi.upperLine)(Condition.AboveThreshold(BigDecimal(rsi.upperLine), currRsi))
    lazy val above = Option.when(currRsi < rsi.lowerLine)(Condition.BelowThreshold(BigDecimal(rsi.lowerLine), currRsi))
    below
      .orElse(above)
      .map(c => Signal(uid, data.currencyPair, rsi.indicator, c, data.prices.head.time))
  }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
