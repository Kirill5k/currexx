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
  case CrossingUp   // TODO: deprecated; delete
  case CrossingDown // TODO: deprecated; delete
  case AboveThreshold(threshold: BigDecimal, value: BigDecimal)
  case BelowThreshold(threshold: BigDecimal, value: BigDecimal)
  case TrendDirectionChange(from: Direction, to: Direction, previousTrendLength: Option[Int] = None)

object Condition {
  given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
    mappings = Map(
      "upper-band-crossing"    -> JsonTaggedAdt.tagged[Condition.UpperBandCrossing],
      "lower-band-crossing"    -> JsonTaggedAdt.tagged[Condition.LowerBandCrossing],
      "lines-crossing"         -> JsonTaggedAdt.tagged[Condition.LinesCrossing],
      "crossing-up"            -> JsonTaggedAdt.tagged[Condition.CrossingUp.type],   // TODO: deprecated; delete
      "crossing-down"          -> JsonTaggedAdt.tagged[Condition.CrossingDown.type], // TODO: deprecated; delete
      "above-threshold"        -> JsonTaggedAdt.tagged[Condition.AboveThreshold],
      "below-threshold"        -> JsonTaggedAdt.tagged[Condition.BelowThreshold],
      "trend-direction-change" -> JsonTaggedAdt.tagged[Condition.TrendDirectionChange]
    ),
    strict = true,
    typeFieldName = "kind"
  )

  // TODO: Revalidate this
  def trendDirectionChange(line: List[Double]): Option[Condition] = {
    val current  = line.toArray
    val previous = current.tail

    val diff                        = previous.lazyZip(current).map((p, c) => p - 2 * c)
    val isGrowing: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isDeclining: Int => Boolean = i => diff(i) < diff(i + 1) && diff(i + 1) < diff(i + 2)

    val trend = LazyList
      .range(0, line.size - 3)
      .map { i =>
        if (!isGrowing(i) && !isDeclining(i)) Direction.Still
        else if (current(i) > previous(i)) Direction.Upward
        else Direction.Downward
      }

    val currTrend = trend.head
    val prevTrend = trend.drop(1).head
    Option
      .when(currTrend != prevTrend)(
        Condition.TrendDirectionChange(prevTrend, currTrend, Some(trend.drop(1).takeWhile(_ == prevTrend).size))
      )
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
