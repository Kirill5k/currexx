package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import currexx.domain.user.UserId
import currexx.calculations.{Filters, MomentumOscillators, MovingAverages}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.common.time.*
import currexx.core.settings.TriggerFrequency
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.market.{Condition, CurrencyPair, Indicator, MarketTimeSeriesData, MovingAverage, ValueSource as VS, ValueTransformation as VT}
import fs2.Stream

import java.time.Instant
import scala.util.{Failure, Success, Try}

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] = signalRepo.getAll(uid, sp)
  override def submit(signal: Signal): F[Unit]                        = save(signal.userId, signal.currencyPair, List(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    settingsRepo
      .get(uid)
      .flatMap { settings =>
        settings.indicators
          .flatMap {
            case tcd: Indicator.TrendChangeDetection => SignalService.detectTrendChange(uid, data, tcd)
            case tc: Indicator.ThresholdCrossing     => SignalService.detectThresholdCrossing(uid, data, tc)
            case ls: Indicator.LinesCrossing         => SignalService.detectLinesCrossing(uid, data, ls)
          }
          .traverse { signal =>
            settings.triggerFrequency match
              case TriggerFrequency.Continuously => F.pure(Some(signal))
              case TriggerFrequency.OncePerDay   => signalRepo.isFirstOfItsKindForThatDate(signal).map(Option.when(_)(signal))
          }
          .map(_.flatten)
      }
      .flatMap(signals => F.whenA(signals.nonEmpty)(save(uid, data.currencyPair, signals)))

  private def save(uid: UserId, cp: CurrencyPair, signals: List[Signal]) =
    signalRepo.saveAll(signals) >> dispatcher.dispatch(Action.ProcessSignals(uid, cp, signals))
}

object SignalService:

  extension (vs: VS)
    private def extract(data: MarketTimeSeriesData): List[Double] = {
      val hour   = data.prices.head.time.hour
      val prices = if (hour > 1 && hour < 12) data.prices.tail else data.prices.toList
      vs match
        case VS.Close => prices.map(_.close)
        case VS.Open  => prices.map(_.open)
        case VS.HL2   => prices.map(p => (p.high + p.low) / 2)
        case VS.HLC3  => prices.map(p => (p.high + p.low + p.close) / 3)
    }

  extension (vt: VT)
    private def transform(data: List[Double], ref: MarketTimeSeriesData): List[Double] =
      vt match
        case VT.Sequenced(transformations)            => transformations.foldLeft(data)((d, t) => t.transform(d, ref))
        case VT.Kalman(gain)                          => Filters.kalman(data, gain)
        case VT.RSX(length)                           => MomentumOscillators.jurikRelativeStrengthIndex(data, length)
        case VT.STOCH(length)                         => MomentumOscillators.stochastic(data, ref.highs, ref.lows, length)
        case VT.WMA(length)                           => MovingAverages.weighted(data, length)
        case VT.SMA(length)                           => MovingAverages.simple(data, length)
        case VT.EMA(length)                           => MovingAverages.exponential(data, length)
        case VT.HMA(length)                           => MovingAverages.hull(data, length)
        case VT.JMA(length, phase, power)             => MovingAverages.jurikSimplified(data, length, phase, power)
        case VT.NMA(length, signalLength, lambda, ma) => MovingAverages.nyquist(data, length, signalLength, lambda, ma.calculation)

  extension (ma: MovingAverage)
    private def calculation: (List[Double], Int) => List[Double] =
      ma match
        case MovingAverage.Exponential => (values, length) => MovingAverages.exponential(values, length)
        case MovingAverage.Simple      => MovingAverages.simple
        case MovingAverage.Weighted    => MovingAverages.weighted
        case MovingAverage.Hull        => MovingAverages.hull

  def detectThresholdCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ThresholdCrossing): Option[Signal] = {
    val source      = indicator.source.extract(data)
    val transformed = indicator.transformation.transform(source, data)
    Condition
      .thresholdCrossing(transformed, indicator.lowerBoundary, indicator.upperBoundary)
      .map(cond => Signal(uid, data.currencyPair, cond, indicator, data.prices.head.time))
  }

  def detectTrendChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.TrendChangeDetection): Option[Signal] = {
    val source      = indicator.source.extract(data)
    val transformed = indicator.transformation.transform(source, data)
    Condition
      .trendDirectionChange(transformed)
      .map(cond => Signal(uid, data.currencyPair, cond, indicator, data.prices.head.time))
  }

  def detectLinesCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.LinesCrossing): Option[Signal] = {
    val source = indicator.source.extract(data)
    val line1  = indicator.line1Transformation.transform(source, data)
    val line2  = indicator.line2Transformation.transform(source, data)
    Condition
      .linesCrossing(line1, line2)
      .map(cond => Signal(uid, data.currencyPair, cond, indicator, data.prices.head.time))
  }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
