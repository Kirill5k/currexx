package currexx.core.signal

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData}
import currexx.domain.signal.{CombinationLogic, Condition, Indicator}

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData, transformer: ValueTransformer = ValueTransformer.pure): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] = signalRepo.getAll(uid, sp)
  override def submit(signal: Signal): F[Unit] = saveAndDispatchAction(signal.userId, signal.currencyPair, List(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData, transformer: ValueTransformer): F[Unit] =
    for
      settings <- settingsRepo.get(uid)
      signals = settings.indicators.flatMap(SignalService.detectSignal(uid, data, transformer))
      _ <- F.whenA(signals.nonEmpty)(saveAndDispatchAction(uid, data.currencyPair, signals))
    yield ()

  private def saveAndDispatchAction(uid: UserId, cp: CurrencyPair, signals: List[Signal]) =
    signalRepo.saveAll(signals) >>
      dispatcher.dispatch(Action.ProcessSignals(uid, cp, signals))
}

object SignalService {

  def detectSignal(uid: UserId, data: MarketTimeSeriesData, transformer: ValueTransformer)(indicator: Indicator): Option[Signal] =
    indicator match
      case vt: Indicator.ValueTracking              => detectValue(uid, data, vt, transformer)
      case tcd: Indicator.TrendChangeDetection      => detectTrendChange(uid, data, tcd, transformer)
      case tc: Indicator.ThresholdCrossing          => detectThresholdCrossing(uid, data, tc, transformer)
      case lc: Indicator.LinesCrossing              => detectLinesCrossing(uid, data, lc, transformer)
      case kc: Indicator.KeltnerChannel             => detectBarrierCrossing(uid, data, kc, transformer)
      case vrd: Indicator.VolatilityRegimeDetection => detectVolatilityRegimeChange(uid, data, vrd, transformer)
      case c: Indicator.Composite                   => detectComposite(uid, data, c, transformer)

  def detectThresholdCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.ThresholdCrossing,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
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

  def detectTrendChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.TrendChangeDetection,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
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

  def detectLinesCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.LinesCrossing,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val source = transformer.extractFrom(data, indicator.source)
    val line1  = transformer.transformTo(source, data, indicator.line1Transformation)
    val line2  = transformer.transformTo(source, data, indicator.line2Transformation)
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

  def detectBarrierCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.KeltnerChannel,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val source    = transformer.extractFrom(data, indicator.source)
    val line1     = transformer.transformTo(source, data, indicator.line1Transformation)
    val line2     = transformer.transformTo(source, data, indicator.line2Transformation)
    val atr       = transformer.averageTrueRange(source, data, indicator.atrLength)
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
      indicator: Indicator.VolatilityRegimeDetection,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val atrLine   = transformer.averageTrueRange(data.closings, data, indicator.atrLength)
    val atrMaLine = transformer.transformTo(atrLine, data, indicator.smoothingType)
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

  def detectValue(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.ValueTracking,
      transformer: ValueTransformer
  ): Option[Signal] = {
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
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

  def detectComposite(
      uid: UserId,
      data: MarketTimeSeriesData,
      composite: Indicator.Composite,
      transformer: ValueTransformer
  ): Option[Signal] =
    val childSignals   = composite.indicators.toList.flatMap(detectSignal(uid, data, transformer))
    val isConditionMet = composite.combinator match
      case CombinationLogic.All => childSignals.size == composite.indicators.size
      case CombinationLogic.Any => childSignals.nonEmpty
    Option
      .when(isConditionMet) {
        Signal(
          userId = uid,
          currencyPair = data.currencyPair,
          interval = data.interval,
          condition = Condition.Composite(NonEmptyList.fromListUnsafe(childSignals.map(_.condition))),
          triggeredBy = composite,
          time = data.prices.head.time
        )
      }

  def make[F[_]: Concurrent](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))

}
