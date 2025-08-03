package currexx.core.signal

import cats.data.NonEmptyList
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.signal.{CombinationLogic, Condition, Indicator}
import currexx.domain.user.UserId

trait SignalDetector:
  def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal]
  def detectThresholdCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ThresholdCrossing): Option[Signal]
  def detectTrendChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.TrendChangeDetection): Option[Signal]
  def detectLinesCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.LinesCrossing): Option[Signal]
  def detectBarrierCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.KeltnerChannel): Option[Signal]
  def detectVolatilityRegimeChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.VolatilityRegimeDetection): Option[Signal]
  def detectValue(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ValueTracking): Option[Signal]
  def detectComposite(uid: UserId, data: MarketTimeSeriesData, composite: Indicator.Composite): Option[Signal]

final private class PureSignalDetector extends SignalDetector {
  private val transformer: ValueTransformer = ValueTransformer.pure

  private def makeSignal(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator)(cond: Condition): Signal =
    Signal(
      userId = uid,
      currencyPair = data.currencyPair,
      interval = data.interval,
      condition = cond,
      triggeredBy = indicator,
      time = data.prices.head.time
    )

  override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] =
    indicator match
      case vt: Indicator.ValueTracking              => detectValue(uid, data, vt)
      case tcd: Indicator.TrendChangeDetection      => detectTrendChange(uid, data, tcd)
      case tc: Indicator.ThresholdCrossing          => detectThresholdCrossing(uid, data, tc)
      case lc: Indicator.LinesCrossing              => detectLinesCrossing(uid, data, lc)
      case kc: Indicator.KeltnerChannel             => detectBarrierCrossing(uid, data, kc)
      case vrd: Indicator.VolatilityRegimeDetection => detectVolatilityRegimeChange(uid, data, vrd)
      case c: Indicator.Composite                   => detectComposite(uid, data, c)

  def detectThresholdCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.ThresholdCrossing
  ): Option[Signal] =
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
    Condition
      .thresholdCrossing(transformed, indicator.lowerBoundary, indicator.upperBoundary)
      .map(makeSignal(uid, data, indicator))

  def detectTrendChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.TrendChangeDetection
  ): Option[Signal] =
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
    Condition
      .trendDirectionChange(transformed)
      .map(makeSignal(uid, data, indicator))

  def detectLinesCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.LinesCrossing
  ): Option[Signal] =
    val source = transformer.extractFrom(data, indicator.source)
    val line1  = transformer.transformTo(source, data, indicator.line1Transformation)
    val line2  = transformer.transformTo(source, data, indicator.line2Transformation)
    Condition
      .linesCrossing(line1, line2)
      .map(makeSignal(uid, data, indicator))

  def detectBarrierCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.KeltnerChannel
  ): Option[Signal] =
    val source    = transformer.extractFrom(data, indicator.source)
    val line1     = transformer.transformTo(source, data, indicator.line1Transformation)
    val line2     = transformer.transformTo(source, data, indicator.line2Transformation)
    val atr       = transformer.averageTrueRange(source, data, indicator.atrLength)
    val upperBand = line1.lazyZip(atr).map((l1, a) => l1 + (a * indicator.atrMultiplier))
    val lowerBand = line1.lazyZip(atr).map((l1, a) => l1 - (a * indicator.atrMultiplier))
    Condition
      .barrierCrossing(line2, upperBand, lowerBand)
      .orElse(Condition.linesCrossing(line1, line2))
      .map(makeSignal(uid, data, indicator))

  def detectVolatilityRegimeChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.VolatilityRegimeDetection
  ): Option[Signal] =
    val atrLine   = transformer.averageTrueRange(data.closings, data, indicator.atrLength)
    val atrMaLine = transformer.transformTo(atrLine, data, indicator.smoothingType)
    Condition
      .volatilityRegimeChange(atrLine, atrMaLine)
      .map(makeSignal(uid, data, indicator))

  def detectValue(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.ValueTracking
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
      composite: Indicator.Composite
  ): Option[Signal] =
    val childSignals   = composite.indicators.toList.flatMap(detect(uid, data))
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
}

final private class CachedSignalDetector(
    private val cache: collection.mutable.Map[String, Option[Signal]]
) extends SignalDetector {
  private val detector = new PureSignalDetector()

  private def cacheKey(data: MarketTimeSeriesData, indicator: Indicator): String =
    s"${data.currencyPair}-${data.interval}-${data.prices.head.time}-$indicator"

  override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] =
    cache.getOrElseUpdate(cacheKey(data, indicator), detector.detect(uid, data)(indicator))

  def detectThresholdCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ThresholdCrossing): Option[Signal] =
    detector.detectThresholdCrossing(uid, data, indicator)

  def detectTrendChange(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.TrendChangeDetection): Option[Signal] =
    detector.detectTrendChange(uid, data, indicator)

  def detectLinesCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.LinesCrossing): Option[Signal] =
    detector.detectLinesCrossing(uid, data, indicator)

  def detectBarrierCrossing(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.KeltnerChannel): Option[Signal] =
    detector.detectBarrierCrossing(uid, data, indicator)

  def detectVolatilityRegimeChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.VolatilityRegimeDetection
  ): Option[Signal] =
    detector.detectVolatilityRegimeChange(uid, data, indicator)

  def detectValue(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator.ValueTracking): Option[Signal] =
    detector.detectValue(uid, data, indicator)

  def detectComposite(uid: UserId, data: MarketTimeSeriesData, composite: Indicator.Composite): Option[Signal] =
    detector.detectComposite(uid, data, composite)
}

object SignalDetector:
  def pure: SignalDetector   = new PureSignalDetector()
  def cached: SignalDetector = new CachedSignalDetector(collection.mutable.Map.empty)
