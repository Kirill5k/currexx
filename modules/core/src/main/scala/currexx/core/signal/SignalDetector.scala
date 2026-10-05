package currexx.core.signal

import cats.data.NonEmptyList
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import currexx.calculations.Statistics
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData}
import currexx.domain.signal.{CombinationLogic, Condition, Indicator}

import java.time.Instant
import currexx.domain.user.UserId

trait SignalDetector:
  def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal]

final private class PureSignalDetector extends SignalDetector {
  private def makeSignal(uid: UserId, window: NumericalWindow, indicator: Indicator)(cond: Condition): Signal =
    val data = window.data
    Signal(
      userId = uid,
      currencyPair = data.currencyPair,
      interval = data.interval,
      condition = cond,
      triggeredBy = indicator,
      time = data.latestTime
    )

  override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] =
    detect(uid, new NumericalWindow(data))(indicator)

  private def detect(uid: UserId, window: NumericalWindow)(indicator: Indicator): Option[Signal] =
    indicator match
      case vt: Indicator.ValueTracking              => detectValue(uid, window, vt)
      case tcd: Indicator.TrendChangeDetection      => detectTrendChange(uid, window, tcd)
      case tc: Indicator.ThresholdCrossing          => detectThresholdCrossing(uid, window, tc)
      case lc: Indicator.LinesCrossing              => detectLinesCrossing(uid, window, lc)
      case kc: Indicator.KeltnerChannel             => detectBarrierCrossing(uid, window, kc)
      case vrd: Indicator.VolatilityRegimeDetection => detectVolatilityRegimeChange(uid, window, vrd)
      case c: Indicator.Composite                   => detectComposite(uid, window, c)
      case plc: Indicator.PriceLineCrossing         => detectPriceLineCrossing(uid, window, plc)
      case bb: Indicator.BollingerBands             => detectBollingerBandsCrossing(uid, window, bb)

  private def detectThresholdCrossing(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.ThresholdCrossing
  ): Option[Signal] =
    val source      = ValueTransformer.extractFrom(window, indicator.source)
    val transformed = ValueTransformer.transformTo(source, window, indicator.transformation)
    Condition
      .thresholdCrossing(transformed, indicator.lowerBoundary, indicator.upperBoundary)
      .map(makeSignal(uid, window, indicator))

  private def detectTrendChange(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.TrendChangeDetection
  ): Option[Signal] =
    val source      = ValueTransformer.extractFrom(window, indicator.source)
    val transformed = ValueTransformer.transformTo(source, window, indicator.transformation)
    Condition
      .trendDirectionChange(transformed)
      .map(makeSignal(uid, window, indicator))

  private def detectLinesCrossing(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.LinesCrossing
  ): Option[Signal] =
    val source = ValueTransformer.extractFrom(window, indicator.source)
    val line1  = ValueTransformer.transformTo(source, window, indicator.line1Transformation)
    val line2  = ValueTransformer.transformTo(source, window, indicator.line2Transformation)
    Condition
      .linesCrossing(line1, line2)
      .map(makeSignal(uid, window, indicator))

  private def detectBarrierCrossing(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.KeltnerChannel
  ): Option[Signal] =
    val priceLine              = ValueTransformer.extractFrom(window, indicator.source)
    val middleBand             = ValueTransformer.transformTo(priceLine, window, indicator.middleBand)
    val atrLine                = ValueTransformer.averageTrueRange(window.closings, window, indicator.atrLength)
    val (upperBand, lowerBand) = bands(middleBand, atrLine, indicator.atrMultiplier)
    Condition
      .bandCrossing(priceLine, upperBand, lowerBand)
      .map(makeSignal(uid, window, indicator))

  private def detectVolatilityRegimeChange(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.VolatilityRegimeDetection
  ): Option[Signal] =
    val atrLine   = ValueTransformer.averageTrueRange(window.closings, window, indicator.atrLength)
    val atrMaLine = ValueTransformer.transformTo(atrLine, window, indicator.smoothingType)
    Condition
      .volatilityRegimeChange(atrLine, atrMaLine)
      .map(makeSignal(uid, window, indicator))

  private def detectValue(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.ValueTracking
  ): Option[Signal] = {
    val source      = ValueTransformer.extractFrom(window, indicator.source)
    val transformed = ValueTransformer.transformTo(source, window, indicator.transformation)
    transformed.headOption.map { latestValue =>
      makeSignal(uid, window, indicator)(Condition.ValueUpdated(indicator.role, BigDecimal.valueOf(latestValue)))
    }
  }

  private def detectComposite(
      uid: UserId,
      window: NumericalWindow,
      composite: Indicator.Composite
  ): Option[Signal] =
    val childSignals   = composite.indicators.toList.flatMap(detect(uid, window))
    val isConditionMet = composite.combinator match
      case CombinationLogic.All => childSignals.size == composite.indicators.size
      case CombinationLogic.Any => childSignals.nonEmpty
    Option
      .when(isConditionMet) {
        makeSignal(uid, window, composite)(Condition.Composite(NonEmptyList.fromListUnsafe(childSignals.map(_.condition))))
      }

  private def detectPriceLineCrossing(
      uid: UserId,
      window: NumericalWindow,
      plc: Indicator.PriceLineCrossing
  ): Option[Signal] =
    val priceLine = ValueTransformer.extractFrom(window, plc.source)
    val otherLine = ValueTransformer.transformTo(priceLine, window, plc.transformation)
    Condition.priceCrossedLine(priceLine, otherLine, plc.role).map(makeSignal(uid, window, plc))

  private def detectBollingerBandsCrossing(
      uid: UserId,
      window: NumericalWindow,
      indicator: Indicator.BollingerBands
  ): Option[Signal] =
    val priceLine              = ValueTransformer.extractFrom(window, indicator.source)
    val middleBandLine         = ValueTransformer.transformTo(priceLine, window, indicator.middleBand)
    val stdDevLine             = Statistics.standardDeviation(priceLine, indicator.stdDevLength)
    val (upperBand, lowerBand) = bands(middleBandLine, stdDevLine, indicator.stdDevMultiplier)
    Condition
      .bandCrossing(priceLine, upperBand, lowerBand)
      .map(makeSignal(uid, window, indicator))

  // bandCrossing reads only the latest two points.
  private def bands(middle: Array[Double], width: Array[Double], multiplier: Double): (Array[Double], Array[Double]) = {
    val size  = math.min(2, math.min(middle.length, width.length))
    val upper = new Array[Double](size)
    val lower = new Array[Double](size)
    var i     = 0
    while (i < size) {
      val offset = width(i) * multiplier
      upper(i) = middle(i) + offset
      lower(i) = middle(i) - offset
      i += 1
    }
    (upper, lower)
  }

}

final private case class CacheKey(
    currencyPair: CurrencyPair,
    interval: Interval,
    latestTime: Instant,
    indicator: Indicator
)

final private class CachedSignalDetector(
    private val cache: Cache[CacheKey, Option[Signal]]
) extends SignalDetector {
  private val detector = new PureSignalDetector()

  override def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal] =
    val key = CacheKey(data.currencyPair, data.interval, data.latestTime, indicator)
    cache.get(key, _ => detector.detect(uid, data)(indicator)).map(_.copy(userId = uid))
}

object SignalDetector:
  def pure: SignalDetector = PureSignalDetector()

  def cached: SignalDetector = cached(maxSizeMB = 1024)

  def cached(maxSizeMB: Int): SignalDetector =
    // Use entry-count eviction: accurately sizing an Indicator/Signal object graph
    // requires a heap-walking tool. A flat byte estimate ignores the key (which contains
    // a deep Indicator ADT) and the Signal's own Indicator + Condition fields, so
    // maximumWeight would give a false bound. Instead, cap by entry count.
    // Each entry is (CurrencyPair, Interval, Instant, Indicator) → Option[Signal].
    // In practice there are O(monitors × indicators) distinct keys per time tick,
    // so 100 000 entries comfortably covers real-world loads within a few hundred MB.
    val maxEntries = (maxSizeMB * 1024L * 1024L) / 1024 // ~1 KB per entry as a rough heuristic
    val cache      = Caffeine
      .newBuilder()
      .maximumSize(maxEntries)
      .build[CacheKey, Option[Signal]]()
    CachedSignalDetector(cache)
