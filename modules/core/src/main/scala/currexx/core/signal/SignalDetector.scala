package currexx.core.signal

import cats.data.NonEmptyList
import com.github.benmanes.caffeine.cache.{Cache, Caffeine}
import currexx.calculations.{Statistics, Volatility}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData}
import currexx.domain.signal.{CombinationLogic, Condition, Indicator}

import java.time.Instant
import currexx.domain.user.UserId

trait SignalDetector:
  def detect(uid: UserId, data: MarketTimeSeriesData)(indicator: Indicator): Option[Signal]

final private class PureSignalDetector extends SignalDetector {
  private val transformer: ValueTransformer = ValueTransformer.pure

  private def makeSignal(uid: UserId, data: MarketTimeSeriesData, indicator: Indicator)(cond: Condition): Signal =
    Signal(
      userId = uid,
      currencyPair = data.currencyPair,
      interval = data.interval,
      condition = cond,
      triggeredBy = indicator,
      time = data.latestTime
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
      case plc: Indicator.PriceLineCrossing         => detectPriceLineCrossing(uid, data, plc)
      case bb: Indicator.BollingerBands             => detectBollingerBandsCrossing(uid, data, bb)

  private def detectThresholdCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.ThresholdCrossing
  ): Option[Signal] =
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
    Condition
      .thresholdCrossing(transformed, indicator.lowerBoundary, indicator.upperBoundary)
      .map(makeSignal(uid, data, indicator))

  private def detectTrendChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.TrendChangeDetection
  ): Option[Signal] =
    val source      = transformer.extractFrom(data, indicator.source)
    val transformed = transformer.transformTo(source, data, indicator.transformation)
    Condition
      .trendDirectionChange(transformed)
      .map(makeSignal(uid, data, indicator))

  private def detectLinesCrossing(
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

  private def detectBarrierCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.KeltnerChannel
  ): Option[Signal] =
    // 1. Get the raw price line that we will check against the bands.
    // This is the primary input for the indicator.
    val priceLine = transformer.extractFrom(data, indicator.source)
    // 2. Calculate the middle band (e.g., EMA of the priceLine).
    val middleBand = transformer.transformTo(priceLine, data, indicator.middleBand)
    // 3. Calculate ATR using the required High, Low, and Close data directly.
    val atrLine = Volatility.averageTrueRange(data.closings, data.highs, data.lows, indicator.atrLength)
    // 4. Calculate the upper and lower bands based on the middle band and ATR.
    val upperBand = middleBand.lazyZip(atrLine).map((mid, atr) => mid + (atr * indicator.atrMultiplier)).toList
    val lowerBand = middleBand.lazyZip(atrLine).map((mid, atr) => mid - (atr * indicator.atrMultiplier)).toList
    // 5. CORRECT: Check if the `priceLine` crosses the calculated bands.
    Condition
      .bandCrossing(priceLine, upperBand, lowerBand)
      .map(makeSignal(uid, data, indicator))

  private def detectVolatilityRegimeChange(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.VolatilityRegimeDetection
  ): Option[Signal] =
    val atrLine   = transformer.averageTrueRange(data.closings, data, indicator.atrLength)
    val atrMaLine = transformer.transformTo(atrLine, data, indicator.smoothingType)
    Condition
      .volatilityRegimeChange(atrLine, atrMaLine)
      .map(makeSignal(uid, data, indicator))

  private def detectValue(
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
        condition = Condition.ValueUpdated(indicator.role, BigDecimal.valueOf(latestValue)),
        triggeredBy = indicator,
        time = data.latestTime
      )
    }
  }

  private def detectComposite(
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
          time = data.latestTime
        )
      }

  private def detectPriceLineCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      plc: Indicator.PriceLineCrossing
  ): Option[Signal] =
    val priceLine = transformer.extractFrom(data, plc.source)
    val otherLine = transformer.transformTo(priceLine, data, plc.transformation)
    Condition.priceCrossedLine(priceLine, otherLine, plc.role).map(makeSignal(uid, data, plc))

  private def detectBollingerBandsCrossing(
      uid: UserId,
      data: MarketTimeSeriesData,
      indicator: Indicator.BollingerBands
  ): Option[Signal] =
    val priceLine      = transformer.extractFrom(data, indicator.source)
    val middleBandLine = transformer.transformTo(priceLine, data, indicator.middleBand)
    val stdDevLine     = Statistics.standardDeviation(priceLine, indicator.stdDevLength)
    val upperBand      = middleBandLine.lazyZip(stdDevLine).map((mid, stdev) => mid + (stdev * indicator.stdDevMultiplier))
    val lowerBand      = middleBandLine.lazyZip(stdDevLine).map((mid, stdev) => mid - (stdev * indicator.stdDevMultiplier))
    Condition
      .bandCrossing(priceLine, upperBand, lowerBand)
      .map(makeSignal(uid, data, indicator))
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
    val cache = Caffeine
      .newBuilder()
      .maximumWeight(maxSizeMB * 1024L * 1024L)
      .weigher[CacheKey, Option[Signal]]((_, value) =>
        value.map(_ => 600).getOrElse(100) // rough estimate: ~600 bytes per Signal, ~100 for empty
      )
      .build[CacheKey, Option[Signal]]()
    CachedSignalDetector(cache)
