package currexx.domain.signal

import cats.data.NonEmptyList
import currexx.domain.types.EnumType
import kirill5k.common.syntax.option.*
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
  case VolatilityRegimeChanged(from: Option[VolatilityRegime], to: VolatilityRegime)

object Condition {
  given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
    mappings = Map(
      "composite"              -> JsonTaggedAdt.tagged[Condition.Composite],
      "upper-band-crossing"    -> JsonTaggedAdt.tagged[Condition.UpperBandCrossing],
      "lower-band-crossing"    -> JsonTaggedAdt.tagged[Condition.LowerBandCrossing],
      "lines-crossing"         -> JsonTaggedAdt.tagged[Condition.LinesCrossing],
      "threshold-crossing"     -> JsonTaggedAdt.tagged[Condition.ThresholdCrossing],
      "trend-direction-change" -> JsonTaggedAdt.tagged[Condition.TrendDirectionChange]
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
  def linesCrossing(line1: List[Double], line2: List[Double]): Option[Condition] =
    crossingDirection(line1, line2)
      .map(Condition.LinesCrossing(_))

  def barrierCrossing(line: List[Double], upperBarrier: List[Double], lowerBarrier: List[Double]): Option[Condition] =
    crossingDirection(line, upperBarrier)
      .map(Condition.UpperBandCrossing(_))
      .orElse(crossingDirection(line, lowerBarrier).map(Condition.LowerBandCrossing(_)))

  private def crossingDirection(line1: List[Double], line2: List[Double]): Option[Direction] =
    (line1, line2) match
      case (l1c :: l1p :: _, l2c :: l2p :: _) =>
        Option
          .when(l1c >= l2c && l1p < l2p)(Direction.Upward)                  // line1 crossed above line2
          .orElse(Option.when(l1c <= l2c && l1p > l2p)(Direction.Downward)) // line1 crossed below line2
      case _ => None

  /** Detects a significant turn (peak or trough) in a time-series line. This method is more reliable than a simple slope change but has a
    * lag of `lookback` periods.
    *
    * @param line
    *   A list of values sorted from latest to earliest.
    * @param lookback
    *   The number of periods to look before and after the turn point for confirmation. A higher value means more reliability but more lag.
    *   A common value is 2 or 3.
    * @return
    *   An Option[Condition.TrendDirectionChange] if a significant turn was confirmed `lookback` periods ago, otherwise None.
    */
  def trendDirectionChange(line: List[Double], lookback: Int = 1): Option[Condition] = {
    def getDirection(current: Double, previous: Double): Direction =
      if (current > previous) Direction.Upward
      else if (current < previous) Direction.Downward
      else Direction.Still

    def calculateTrendLength(history: List[Double], trendDirection: Direction): Int =
      val historicalSegments   = history.sliding(2)
      val historicalDirections = historicalSegments.collect { case curr :: prev :: _ => getDirection(curr, prev) }
      val trendSegmentCount    = historicalDirections.takeWhile(_ == trendDirection).size
      trendSegmentCount + 1

    // The total window size needed to confirm a turn at the center.
    val windowSize = 2 * lookback + 1
    Option
      .flatWhen(line.length >= windowSize) {
        val window = line.take(windowSize)
        // The point we are testing is the one in the middle of our window.
        // Since the list is latest-to-earliest, this point is `lookback` periods in the past.
        val candidateTurnPoint = window(lookback)
        val beforeTurn         = window.take(lookback)
        val afterTurn          = window.drop(lookback + 1)

        Option
          .when(beforeTurn.forall(_ < candidateTurnPoint) && afterTurn.forall(_ < candidateTurnPoint))(
            Condition.TrendDirectionChange(
              Direction.Upward,
              Direction.Downward,
              Some(calculateTrendLength(line.drop(lookback), Direction.Upward))
            )
          )
          .orElse(
            Option
              .when(beforeTurn.forall(_ > candidateTurnPoint) && afterTurn.forall(_ > candidateTurnPoint))(
                Condition.TrendDirectionChange(
                  Direction.Downward,
                  Direction.Upward,
                  Some(calculateTrendLength(line.drop(lookback), Direction.Downward))
                )
              )
          )
      }
  }

  def thresholdCrossing(line: List[Double], lowerBoundary: Double, upperBoundary: Double): Option[Condition] =
    line match
      case current :: previous :: _ =>
        def checkCrossing(boundary: Double, boundaryType: Boundary): Option[Condition] =
          (current, previous) match
            case (c, p) if c >= boundary && p < boundary =>
              Some(Condition.ThresholdCrossing(boundary, c, Direction.Upward, boundaryType))
            case (c, p) if c < boundary && p >= boundary =>
              Some(Condition.ThresholdCrossing(boundary, c, Direction.Downward, boundaryType))
            case _ => None
        // Check upper boundary first, then lower boundary
        checkCrossing(upperBoundary, Boundary.Upper)
          .orElse(checkCrossing(lowerBoundary, Boundary.Lower))
      case _ => None

  def volatilityRegimeChange(primaryLine: List[Double], smoothedLine: List[Double]): Option[Condition] =
    // Safely pattern match to get the current and previous values from both lines.
    (primaryLine, smoothedLine) match {
      case (currentPrimary :: previousPrimary :: restPrimary, currentSmoothed :: previousSmoothed :: restSmoothed) =>

        val currentRegime = if (currentPrimary > currentSmoothed) VolatilityRegime.High else VolatilityRegime.Low

        // Now, determine the previous regime.
        // We check if there is enough data to have a valid previous state.
        // If `restPrimary` is empty, it means we only have 2 data points, so there is no "from".
        val previousRegimeOpt = Option.when(restPrimary.nonEmpty && restSmoothed.nonEmpty) {
          if (previousPrimary > previousSmoothed) VolatilityRegime.High else VolatilityRegime.Low
        }
        // A signal is generated if:
        // 1. The regime has changed from the previous one.
        // OR
        // 2. There was no previous regime (it's the first ever calculation).
        Option.when(!previousRegimeOpt.contains(currentRegime)) {
          Condition.VolatilityRegimeChanged(from = previousRegimeOpt, to = currentRegime)
        }
      // Not enough data to even determine the current regime.
      case _ => None
    }
}
