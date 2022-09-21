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
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.errors.AppError
import currexx.domain.market.{Condition, CurrencyPair, Indicator, MarketTimeSeriesData, MovingAverage, Trend, ValueSource, ValueTransformation as VT}
import fs2.Stream

import java.time.Instant
import scala.util.{Failure, Success, Try}

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]]
  def getSettings(uid: UserId): F[SignalSettings]
  def updateSettings(settings: SignalSettings): F[Unit]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def getSettings(uid: UserId): F[SignalSettings]            = settingsRepo.get(uid)
  override def updateSettings(settings: SignalSettings): F[Unit]      = settingsRepo.update(settings)
  override def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] = signalRepo.getAll(uid, sp)
  override def submit(signal: Signal): F[Unit]                        = save(signal.userId, signal.currencyPair, List(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    getSettings(uid)
      .flatMap { settings =>
        settings.indicators
          .flatMap {
            case tcd: Indicator.TrendChangeDetection => SignalService.detectTrendChange(uid, data, tcd)
            case tc: Indicator.ThresholdCrossing     => SignalService.detectThresholdCrossing(uid, data, tc)
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

  extension (vs: ValueSource)
    def extract(data: MarketTimeSeriesData): List[Double] =
      vs match
        case ValueSource.Close => data.prices.map(_.close.toDouble).toList
        case ValueSource.Open  => data.prices.map(_.open.toDouble).toList
        case ValueSource.HL2   => data.prices.map(_.high.toDouble).zip(data.prices.map(_.low.toDouble)).map((h, l) => (h + l) / 2).toList

  extension (vt: VT.SingleOutput)
    def transform(data: List[Double]): List[Double] =
      vt match
        case VT.SingleOutput.Sequenced(transformations) => transformations.foldLeft(data)((d, t) => t.transform(d))
        case VT.SingleOutput.Kalman(gain)               => Filters.kalman(data, gain)
        case VT.SingleOutput.WMA(length)                => MovingAverages.weighted(data, length)
        case VT.SingleOutput.SMA(length)                => MovingAverages.simple(data, length)
        case VT.SingleOutput.EMA(length)                => MovingAverages.exponential(data, length)
        case VT.SingleOutput.HMA(length)                => MovingAverages.hull(data, length)
        case VT.SingleOutput.NMA(length, signalLength, lambda, ma) =>
          MovingAverages.nyquist(data, length, signalLength, lambda, ma.calculation)

  extension (ma: MovingAverage)
    def calculation: (List[Double], Int) => List[Double] =
      ma match
        case MovingAverage.Exponential => (values, length) => MovingAverages.exponential(values, length)
        case MovingAverage.Simple      => MovingAverages.simple
        case MovingAverage.Weighted    => MovingAverages.weighted

  def detectThresholdCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ThresholdCrossing): Option[Signal] = {
    val source = indicator.source.extract(data)
    val transformed = indicator.transformation match
      case so: VT.SingleOutput => so.transform(source)
      case VT.DoubleOutput.STOCH(length, slowK, slowD) =>
        val highs = data.prices.map(_.high.toDouble).toList
        val lows  = data.prices.map(_.low.toDouble).toList
        MomentumOscillators.stochastic(source, highs, lows, length, slowK, slowD)._1
    val currentValue = transformed.head
    val condition =
      if (indicator.upperBoundary < currentValue) Some(Condition.AboveThreshold(indicator.upperBoundary, currentValue))
      else if (indicator.lowerBoundary > currentValue) Some(Condition.BelowThreshold(indicator.lowerBoundary, currentValue))
      else None
    condition.map(cond => Signal(uid, data.currencyPair, cond, indicator, data.prices.head.time))
  }

  def detectTrendChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.TrendChangeDetection): Option[Signal] = {
    val source      = indicator.source.extract(data)
    val transformed = indicator.transformation.transform(source)
    val res         = identifyTrends(transformed)
    val prevTrend   = res.drop(1).head
    Option
      .when(res.head != prevTrend)(Condition.TrendDirectionChange(prevTrend, res.head, Some(res.drop(1).takeWhile(_ == prevTrend).size)))
      .map(cond => Signal(uid, data.currencyPair, cond, indicator, data.prices.head.time))
  }

  private def identifyTrends(values: List[Double]): List[Trend] = {
    val vals  = values.toArray
    val vals2 = vals.tail
    val vals3 = vals.zip(vals.map(-_)).map(_ - _)
    val vals4 = vals.zip(vals).map(_ + _)

    val diff  = vals2.zip(vals3).map(_ - _)
    val diff3 = vals4.zip(vals2).map(_ - _)

    val isNotUp: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isNotDown: Int => Boolean = i => diff3(i) > diff3(i + 1) && diff3(i + 1) > diff3(i + 2)

    val trend         = vals.zip(vals2).map((v1, v2) => if (v1 > v2) Trend.Upward else Trend.Downward)
    val consolidation = (0 until values.size - 5).map(i => Option.when(isNotUp(i) == isNotDown(i))(Trend.Consolidation))
    consolidation.zip(trend).map(_.getOrElse(_)).toList
  }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
