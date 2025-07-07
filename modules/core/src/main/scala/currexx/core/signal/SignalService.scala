package currexx.core.signal

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.calculations.{Filters, MomentumOscillators, MovingAverages, Volatility}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData}
import currexx.domain.signal.{Condition, Indicator, MovingAverage, ValueSource as VS, ValueTransformation as VT}

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
  override def submit(signal: Signal): F[Unit] = saveAndDispatchAction(signal.userId, signal.currencyPair, List(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    for
      settings <- settingsRepo.get(uid)
      signals = settings.indicators.flatMap(SignalService.detectSignal(uid, data, _))
      _ <- F.whenA(signals.nonEmpty)(saveAndDispatchAction(uid, data.currencyPair, signals))
    yield ()

  private def saveAndDispatchAction(uid: UserId, cp: CurrencyPair, signals: List[Signal]) =
    signalRepo.saveAll(signals) >>
      dispatcher.dispatch(Action.ProcessSignals(uid, cp, signals))
}

object SignalService {

  def detectSignal(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator): Option[Signal] =
    indicator match {
      case vt: Indicator.ValueTracking              => detectValue(uid, data, vt)
      case tcd: Indicator.TrendChangeDetection      => detectTrendChange(uid, data, tcd)
      case tc: Indicator.ThresholdCrossing          => detectThresholdCrossing(uid, data, tc)
      case lc: Indicator.LinesCrossing              => detectLinesCrossing(uid, data, lc)
      case kc: Indicator.KeltnerChannel             => detectBarrierCrossing(uid, data, kc)
      case vrd: Indicator.VolatilityRegimeDetection => detectVolatilityRegimeChange(uid, data, vrd)
      case c: Indicator.Composite                   =>
        val childSignalOptions = c.indicators.map(childInd => detectSignal(uid, data, childInd))
        Option
          .when(childSignalOptions.forall(_.isDefined)) {
            val childSignals = childSignalOptions.toList.flatten
            Signal(
              userId = uid,
              currencyPair = data.currencyPair,
              interval = data.interval,
              condition = Condition.Composite(NonEmptyList.fromListUnsafe(childSignals.map(_.condition))),
              triggeredBy = c, // The parent indicator
              time = data.prices.head.time
            )
          }
    }

  extension (vs: VS)
    private def extract(data: MarketTimeSeriesData): List[Double] = {
      val prices = data.prices.toList
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
      .map { cond =>
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = cond,
          triggeredBy = indicator,
          time = data.prices.head.time
        )
      }
  }

  def detectTrendChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.TrendChangeDetection): Option[Signal] = {
    val source      = indicator.source.extract(data)
    val transformed = indicator.transformation.transform(source, data)
    Condition
      .trendDirectionChange(transformed)
      .map { cond =>
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = cond,
          triggeredBy = indicator,
          time = data.prices.head.time
        )
      }
  }

  def detectLinesCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.LinesCrossing): Option[Signal] = {
    val source = indicator.source.extract(data)
    val line1  = indicator.line1Transformation.transform(source, data)
    val line2  = indicator.line2Transformation.transform(source, data)
    Condition
      .linesCrossing(line1, line2)
      .map { cond =>
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = cond,
          triggeredBy = indicator,
          time = data.prices.head.time
        )
      }
  }

  def detectBarrierCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.KeltnerChannel): Option[Signal] = {
    val source    = indicator.source.extract(data)
    val line1     = indicator.line1Transformation.transform(source, data)
    val line2     = indicator.line2Transformation.transform(source, data)
    val atr       = Volatility.averageTrueRange(source, data.highs, data.lows, indicator.atrLength)
    val upperBand = line1.lazyZip(atr).map((l1, a) => l1 + (a * indicator.atrMultiplier))
    val lowerBand = line1.lazyZip(atr).map((l1, a) => l1 - (a * indicator.atrMultiplier))
    Condition
      .barrierCrossing(line2, upperBand, lowerBand)
      .orElse(Condition.linesCrossing(line1, line2))
      .map { cond =>
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = cond,
          triggeredBy = indicator,
          time = data.prices.head.time
        )
      }
  }

  def detectVolatilityRegimeChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.VolatilityRegimeDetection
  ): Option[Signal] = {
    val atrLine   = Volatility.averageTrueRange(data.closings, data.highs, data.lows, indicator.atrLength)
    val atrMaLine = indicator.smoothingType.transform(atrLine, data)
    Condition
      .volatilityRegimeChange(atrLine, atrMaLine)
      .map { condition => // If a condition was returned, wrap it in a Signal.
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = condition,
          triggeredBy = indicator,
          time = data.prices.head.time
        )
      }
  }

  def detectValue(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ValueTracking): Option[Signal] = {
    val source      = indicator.source.extract(data)
    val transformed = indicator.transformation.transform(source, data)
    transformed.headOption.map { latestValue =>
      Signal(
        userId = uid,
        currencyPair = data.currencyPair,
        interval = data.interval,
        condition = Condition.ValueUpdated(indicator.role, latestValue),
        triggeredBy = indicator,
        time = data.prices.head.time
      )
    }
  }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))

}
