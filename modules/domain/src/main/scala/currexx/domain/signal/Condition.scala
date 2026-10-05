package currexx.domain.signal

import cats.data.NonEmptyList
import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object VolatilityRegime extends EnumType[VolatilityRegime](() => VolatilityRegime.values)
enum VolatilityRegime:
  case High, Low

object Direction extends EnumType[Direction](() => Direction.values)
enum Direction:
  case Upward, Downward, Still

object Boundary extends EnumType[Boundary](() => Boundary.values)
enum Boundary:
  case Upper, Lower

enum Condition derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Composite(conditions: NonEmptyList[Condition])
  case UpperBandCrossing(direction: Direction)
  case LowerBandCrossing(direction: Direction)
  case LinesCrossing(direction: Direction)
  case ThresholdCrossing(threshold: BigDecimal, value: BigDecimal, direction: Direction, boundary: Boundary)
  case TrendDirectionChange(from: Direction, to: Direction, previousTrendLength: Option[Int] = None)
  case VolatilityRegimeChange(from: Option[VolatilityRegime], to: VolatilityRegime)
  case ValueUpdated(role: ValueRole, value: BigDecimal)
  case PriceCrossedLine(lineRole: ValueRole, direction: Direction)

object Condition {
  given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
    mappings = Map(
      "composite"                -> JsonTaggedAdt.tagged[Condition.Composite],
      "upper-band-crossing"      -> JsonTaggedAdt.tagged[Condition.UpperBandCrossing],
      "lower-band-crossing"      -> JsonTaggedAdt.tagged[Condition.LowerBandCrossing],
      "lines-crossing"           -> JsonTaggedAdt.tagged[Condition.LinesCrossing],
      "threshold-crossing"       -> JsonTaggedAdt.tagged[Condition.ThresholdCrossing],
      "volatility-regime-change" -> JsonTaggedAdt.tagged[Condition.VolatilityRegimeChange],
      "trend-direction-change"   -> JsonTaggedAdt.tagged[Condition.TrendDirectionChange],
      "value-updated"            -> JsonTaggedAdt.tagged[Condition.ValueUpdated],
      "price-crossed-line"       -> JsonTaggedAdt.tagged[Condition.PriceCrossedLine]
    ),
    strict = true,
    typeFieldName = "kind"
  )

  /** Detects if a crossover occurred between two time-series lines on the most recent data point.
    *
    * This function provides a general-purpose utility for identifying crossover events. The interpretation of the crossover (e.g., as a
    * buy/sell signal) is left to the caller.
    *
    * The direction of the cross is reported from the perspective of `line1`:
    *   - `Upward` cross: `line1` has crossed from being below `line2` to being at or above `line2`.
    *   - `Downward` cross: `line1` has crossed from being above `line2` to being at or below `line2`.
    *
    * Example Use Case: A classic moving average crossover signal could be implemented by the caller like this:
    * {{{
    *   val fastMA = ...
    *   val slowMA = ...
    *   linesCrossing(fastMA, slowMA) match {
    *     case Some(Condition.LinesCrossing(Direction.Upward)) => // Golden Cross -> Potential BUY signal
    *     case Some(Condition.LinesCrossing(Direction.Downward)) => // Death Cross -> Potential SELL signal
    *     case _ => // No signal
    *   }
    * }}}
    *
    * @param line1
    *   The first line (e.g., a fast moving average, or the price line). Data is sorted from latest to earliest.
    * @param line2
    *   The second line (e.g., a slow moving average, or a signal line). Data is sorted from latest to earliest.
    * @return
    *   `Some(Condition.LinesCrossing)` containing the direction of the cross if one occurred. `None` otherwise.
    */
  def linesCrossing(line1: Array[Double], line2: Array[Double]): Option[Condition] =
    crossingDirection(line1, line2).map(Condition.LinesCrossing(_))

  def bandCrossing(line: Array[Double], upperBarrier: Array[Double], lowerBarrier: Array[Double]): Option[Condition] =
    crossingDirection(line, upperBarrier)
      .map(Condition.UpperBandCrossing(_))
      .orElse(crossingDirection(line, lowerBarrier).map(Condition.LowerBandCrossing(_)))

  private def crossingDirection(line1: Array[Double], line2: Array[Double]): Option[Direction] =
    if (line1.length < 2 || line2.length < 2) None
    else if (line1(0) >= line2(0) && line1(1) < line2(1)) Some(Direction.Upward)
    else if (line1(0) <= line2(0) && line1(1) > line2(1)) Some(Direction.Downward)
    else None

  def priceCrossedLine(priceLine: Array[Double], otherLine: Array[Double], lineRole: ValueRole): Option[Condition] =
    crossingDirection(priceLine, otherLine).map(Condition.PriceCrossedLine(lineRole, _))

  /** Detects a significant turn (peak or trough) in a time-series line. This method is more reliable than a simple slope change but has a
    * lag of `lookback` periods.
    *
    * @param line
    *   Values sorted from latest to earliest.
    * @param lookback
    *   The number of periods to look before and after the turn point for confirmation. A higher value means more reliability but more lag.
    *   A common value is 2 or 3.
    * @return
    *   An Option[Condition.TrendDirectionChange] if a significant turn was confirmed `lookback` periods ago, otherwise None.
    */
  def trendDirectionChange(line: Array[Double], lookback: Int = 1): Option[Condition] = {
    require(lookback > 0, "lookback must be positive")
    val windowSize = 2L * lookback + 1
    if (line.length < windowSize) None
    else {
      val candidate = line(lookback)
      var isPeak    = true
      var isTrough  = true
      var i         = 0
      while (i < windowSize && (isPeak || isTrough)) {
        if (i != lookback) {
          isPeak = isPeak && line(i) < candidate
          isTrough = isTrough && line(i) > candidate
        }
        i += 1
      }
      if (isPeak)
        Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(trendLength(line, lookback, Direction.Upward))))
      else if (isTrough)
        Some(Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(trendLength(line, lookback, Direction.Downward))))
      else None
    }
  }

  private def trendLength(line: Array[Double], start: Int, direction: Direction): Int = {
    var i = start
    while (i + 1 < line.length && (if (direction == Direction.Upward) line(i) > line(i + 1) else line(i) < line(i + 1))) i += 1
    i - start + 1
  }

  def thresholdCrossing(line: Array[Double], lowerBoundary: Double, upperBoundary: Double): Option[Condition] =
    if (line.length < 2) None
    else {
      val current  = line(0)
      val previous = line(1)
      // Prioritize entry into the destination zone when one move crosses both boundaries.
      if (current >= upperBoundary && previous < upperBoundary)
        Some(Condition.ThresholdCrossing(BigDecimal.valueOf(upperBoundary), BigDecimal.valueOf(current), Direction.Upward, Boundary.Upper))
      else if (current <= lowerBoundary && previous > lowerBoundary)
        Some(
          Condition.ThresholdCrossing(BigDecimal.valueOf(lowerBoundary), BigDecimal.valueOf(current), Direction.Downward, Boundary.Lower)
        )
      else if (current < upperBoundary && previous >= upperBoundary)
        Some(
          Condition.ThresholdCrossing(BigDecimal.valueOf(upperBoundary), BigDecimal.valueOf(current), Direction.Downward, Boundary.Upper)
        )
      else if (current > lowerBoundary && previous <= lowerBoundary)
        Some(Condition.ThresholdCrossing(BigDecimal.valueOf(lowerBoundary), BigDecimal.valueOf(current), Direction.Upward, Boundary.Lower))
      else None
    }

  def volatilityRegimeChange(primaryLine: Array[Double], smoothedLine: Array[Double]): Option[Condition] =
    if (primaryLine.length < 2 || smoothedLine.length < 2) None
    else {
      val currentRegime  = if (primaryLine(0) > smoothedLine(0)) VolatilityRegime.High else VolatilityRegime.Low
      val previousRegime = Option.when(primaryLine.length > 2 && smoothedLine.length > 2) {
        if (primaryLine(1) > smoothedLine(1)) VolatilityRegime.High else VolatilityRegime.Low
      }
      Option.when(!previousRegime.contains(currentRegime)) {
        Condition.VolatilityRegimeChange(previousRegime, currentRegime)
      }
    }
}
