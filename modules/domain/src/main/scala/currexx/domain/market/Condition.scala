package currexx.domain.market

import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object Direction extends EnumType[Direction](() => Direction.values)
enum Direction:
  case Upward, Downward, Still

enum Condition derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case UpperBandCrossing(direction: Direction)
  case LowerBandCrossing(direction: Direction)
  case LinesCrossing(direction: Direction)
  case AboveThreshold(threshold: BigDecimal, value: BigDecimal)
  case BelowThreshold(threshold: BigDecimal, value: BigDecimal)
  case TrendDirectionChange(from: Direction, to: Direction, previousTrendLength: Option[Int] = None)

object Condition {
  given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
    mappings = Map(
      "upper-band-crossing"    -> JsonTaggedAdt.tagged[Condition.UpperBandCrossing],
      "lower-band-crossing"    -> JsonTaggedAdt.tagged[Condition.LowerBandCrossing],
      "lines-crossing"         -> JsonTaggedAdt.tagged[Condition.LinesCrossing],
      "above-threshold"        -> JsonTaggedAdt.tagged[Condition.AboveThreshold],
      "below-threshold"        -> JsonTaggedAdt.tagged[Condition.BelowThreshold],
      "trend-direction-change" -> JsonTaggedAdt.tagged[Condition.TrendDirectionChange]
    ),
    strict = true,
    typeFieldName = "kind"
  )

  /** Detects if the direction of a time-series line has changed on the most recent data point. If a change is detected, it returns the old
    * direction, the new direction, and the length of the trend that just ended.
    *
    * @param line
    *   A list of values (e.g., smoothed prices) sorted from latest to earliest.
    * @return
    *   An Option[Condition.TrendDirectionChange] if a change occurred on the latest tick, otherwise None.
    */
  def trendDirectionChange(line: List[Double]): Option[Condition] = {
    def getDirection(current: Double, previous: Double): Direction =
      if (current > previous) Direction.Upward
      else if (current < previous) Direction.Downward
      else Direction.Still

    line match
      case latest :: prev1 :: prev2 :: _ =>
        val currentDirection  = getDirection(latest, prev1)
        val previousDirection = getDirection(prev1, prev2)

        // A change occurs if the directions are different.
        // We often want to ignore changes to/from a 'Still' state, but for completeness,
        // this implementation detects all changes. You could add `&& previousDirection != Direction.Still`
        // to be more strict about what constitutes a "trend".
        Option
          .when(currentDirection != previousDirection) {
            val historicalSegments   = line.tail.sliding(2)
            val historicalDirections = historicalSegments.collect { case curr :: prev :: _ => getDirection(curr, prev) }
            val trendSegmentCount    = historicalDirections.takeWhile(_ == previousDirection).size

            Condition.TrendDirectionChange(
              from = previousDirection,
              to = currentDirection,
              previousTrendLength = Some(trendSegmentCount + 1)
            )
          }
      case _ => None
  }

  def thresholdCrossing(line: List[Double], min: Double, max: Double): Option[Condition] =
    (line.head, line.drop(1).head) match
      case (c, p) if c > max && p <= max => Some(Condition.AboveThreshold(max, c))
      case (c, p) if c < min && p >= min => Some(Condition.BelowThreshold(min, c))
      case _                             => None

  private def crossingDirection(line1: List[Double], line2: List[Double]): Option[Direction] =
    (line1.head, line2.head, line1.drop(1).head, line2.drop(1).head) match
      case (l1c, l2c, l1p, l2p) if l1c >= l2c && l1p < l2p => Some(Direction.Upward)
      case (l1c, l2c, l1p, l2p) if l1c <= l2c && l1p > l2p => Some(Direction.Downward)
      case _                                               => None

  // line1=SLOW, line2=FAST
  // CrossingUp=Sell, CrossingDown=Buy
  // TODO: Revalidate this
  def linesCrossing(line1: List[Double], line2: List[Double]): Option[Condition] =
    crossingDirection(line1, line2).map(Condition.LinesCrossing(_))

  def barrierCrossing(line: List[Double], upperBarrier: List[Double], lowerBarrier: List[Double]): Option[Condition] =
    crossingDirection(line, upperBarrier)
      .map(Condition.UpperBandCrossing(_))
      .orElse(crossingDirection(line, lowerBarrier).map(Condition.LowerBandCrossing(_)))
}
