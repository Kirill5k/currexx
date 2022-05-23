package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.calculations.{Filters, MomentumOscillators, MovingAverages}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{Condition, CurrencyPair, Indicator, IndicatorParameters, MarketTimeSeriesData, Trend}
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
        Stream
          .emits(
            settings.indicators.flatMap {
              case macd: IndicatorParameters.MACD        => SignalService.detectMacd(uid, data, macd)
              case rsi: IndicatorParameters.RSI          => SignalService.detectRsi(uid, data, rsi)
              case hma: IndicatorParameters.HMA          => SignalService.detectHma(uid, data, hma)
              case stoch: IndicatorParameters.Stochastic => SignalService.detectStoch(uid, data, stoch)
            }
          )
          .evalFilter { signal =>
            settings.triggerFrequency match
              case TriggerFrequency.Continuously => F.pure(true)
              case TriggerFrequency.OncePerDay   => signalRepo.isFirstOfItsKindForThatDate(signal)
          }
      }
      .evalMap(submit)
      .compile
      .drain
}

object SignalService:

  def detectMacd(uid: UserId, data: MarketTimeSeriesData, macd: IndicatorParameters.MACD): Option[Signal] = {
    val (macdLine, signalLine) = MovingAverages.macdWithSignal(
      values = data.prices.map(_.close.toDouble).toList,
      fastLength = macd.fastLength,
      slowLength = macd.slowLength,
      signalSmoothing = macd.signalSmoothing
    )
    Condition
      .lineCrossing(macdLine.head, signalLine.head, macdLine.drop(1).head, signalLine.drop(1).head)
      .map(c => Signal(uid, data.currencyPair, macd.indicator, c, data.prices.head.time))
  }

  def detectRsi(uid: UserId, data: MarketTimeSeriesData, rsi: IndicatorParameters.RSI): Option[Signal] = {
    val rsis    = MomentumOscillators.relativeStrengthIndex(data.prices.map(_.close.toDouble).toList, rsi.length)
    val currRsi = rsis.head
    def above   = Option.when(currRsi > rsi.upperLine)(Condition.AboveThreshold(BigDecimal(rsi.upperLine), currRsi))
    def below   = Option.when(currRsi < rsi.lowerLine)(Condition.BelowThreshold(BigDecimal(rsi.lowerLine), currRsi))
    below
      .orElse(above)
      .map(c => Signal(uid, data.currencyPair, rsi.indicator, c, data.prices.head.time))
  }

  def detectStoch(uid: UserId, data: MarketTimeSeriesData, stoch: IndicatorParameters.Stochastic): Option[Signal] = {
    val (k, d) = MomentumOscillators.stochastic(
      closings = data.prices.map(_.close.toDouble).toList,
      highs = data.prices.map(_.high.toDouble).toList,
      lows = data.prices.map(_.low.toDouble).toList,
      length = stoch.length,
      slowKLength = stoch.slowKLength,
      slowDLength = stoch.slowDLength
    )
    val (currK, currD) = (k.head, d.head)
    def above = Option.when(currK.min(currD) > stoch.upperLine)(Condition.AboveThreshold(BigDecimal(stoch.upperLine), currK.min(currD)))
    def below = Option.when(currK.max(currD) < stoch.lowerLine)(Condition.BelowThreshold(BigDecimal(stoch.lowerLine), currK.max(currD)))
    below
      .orElse(above)
      .map(c => Signal(uid, data.currencyPair, stoch.indicator, c, data.prices.head.time))
  }

  def detectHma(uid: UserId, data: MarketTimeSeriesData, hma: IndicatorParameters.HMA): Option[Signal] = {
    val values = data.prices.toList.map(_.close.toDouble)
    val kalman = hma.kalmanSmoothing.map(ks => Filters.ghkKalman(values, ks.alpha, ks.betta, ks.gamma, values.last, 0d, 0d, 1))
    val hmas   = MovingAverages.hull(kalman.getOrElse(values), hma.length).toArray.take(5)
    val hmas2  = hmas.tail
    val hmas3  = hmas.zip(hmas.map(-_)).map(_ - _)
    val hmas4  = hmas.zip(hmas).map(_ + _)

    val diff  = hmas2.zip(hmas3).map(_ - _)
    val diff3 = hmas4.zip(hmas2).map(_ - _)

    val isNotUp: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isNotDown: Int => Boolean = i => diff3(i) > diff3(i + 1) && diff3(i + 1) > diff3(i + 2)

    val trend         = hmas.zip(hmas2).map((v1, v2) => if (v1 > v2) Trend.Upward else Trend.Downward)
    val consolidation = (0 until 2).map(i => Option.when(isNotUp(i) == isNotDown(i))(Trend.Consolidation))
    val res           = consolidation.zip(trend).map(_.getOrElse(_))
    Option
      .when(res.head != res(1))(Condition.TrendDirectionChange(res(1), res.head))
      .map(c => Signal(uid, data.currencyPair, hma.indicator, c, data.prices.head.time))
  }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
