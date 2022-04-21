package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.calculations.MovingAverageCalculator
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorParameters, MarketTimeSeriesData}
import fs2.Stream

import scala.util.Try

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]
  def getSettings(uid: UserId, pair: CurrencyPair): F[Option[SignalSettings]]
  def updateSettings(settings: SignalSettings): F[Unit]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def getSettings(uid: UserId, pair: CurrencyPair): F[Option[SignalSettings]] = settingsRepo.get(uid, pair)
  override def updateSettings(settings: SignalSettings): F[Unit]                       = settingsRepo.update(settings)
  override def getAll(uid: UserId): F[List[Signal]]                                    = signalRepo.getAll(uid)
  override def submit(signal: Signal): F[Unit] =
    signalRepo.save(signal) >> dispatcher.dispatch(Action.SignalSubmitted(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    Stream
      .eval(getSettings(uid, data.currencyPair))
      .evalMap {
        case Some(settings) =>
          settings.pure[F]
        case None =>
          val defaultSettings = SignalSettings.default(uid, data.currencyPair)
          settingsRepo.update(defaultSettings).as(defaultSettings)
      }
      .flatMap { settings =>
        Stream.emits(
          settings.indicators.flatMap {
            case macd: IndicatorParameters.MACD => detectMacdCrossing(uid, data, macd)
            case rsi: IndicatorParameters.RSI   => None
          }
        )
      }
      .evalMap(submit)
      .compile
      .drain

  private def detectMacdCrossing(uid: UserId, data: MarketTimeSeriesData, macd: IndicatorParameters.MACD): Option[Signal] = {
    val (macdLine, signalLine) = MovingAverageCalculator.macdWithSignal(
      values = data.prices.map(_.close).toList,
      fastLength = macd.fastLength,
      slowLength = macd.slowLength,
      signalSmoothing = macd.signalSmoothing
    )
    Condition
      .lineCrossing(macdLine.head, signalLine.head, macdLine.drop(1).head, signalLine.drop(1).head)
      .map(c => Signal(uid, data.currencyPair, Indicator.MACD, c, data.prices.head.time))
  }
}

object SignalService:
  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
