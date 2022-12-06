package currexx.domain.market

import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object Trend extends EnumType[Trend](() => Trend.values)
enum Trend:
  case Upward, Downward, Consolidation

enum Condition derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case CrossingUp
  case CrossingDown
  case AboveThreshold(threshold: BigDecimal, value: BigDecimal)
  case BelowThreshold(threshold: BigDecimal, value: BigDecimal)
  case TrendDirectionChange(from: Trend, to: Trend, previousTrendLength: Option[Int] = None)

object Condition {
  given JsonTaggedAdt.Config[Condition] = JsonTaggedAdt.Config.Values[Condition](
    mappings = Map(
      "crossing-up"            -> JsonTaggedAdt.tagged[Condition.CrossingUp.type],
      "crossing-down"          -> JsonTaggedAdt.tagged[Condition.CrossingDown.type],
      "above-threshold"        -> JsonTaggedAdt.tagged[Condition.AboveThreshold],
      "below-threshold"        -> JsonTaggedAdt.tagged[Condition.BelowThreshold],
      "trend-direction-change" -> JsonTaggedAdt.tagged[Condition.TrendDirectionChange]
    ),
    strict = true,
    typeFieldName = "kind"
  )

  def trendDirectionChange(line: List[Double]): Option[Condition] = {
    val current  = line.toArray
    val previous = current.tail

    val diff                        = previous.lazyZip(current).map((p, c) => p - 2 * c)
    val isGrowing: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isDeclining: Int => Boolean = i => diff(i) < diff(i + 1) && diff(i + 1) < diff(i + 2)

    val trend = LazyList
      .range(0, line.size - 3)
      .map { i =>
        if (!isGrowing(i) && !isDeclining(i)) Trend.Consolidation
        else if (current(i) > previous(i)) Trend.Upward
        else Trend.Downward
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

  // line1=SLOW, line2=FAST
  // CrossingUp=Sell, CrossingDown=Buy
  def linesCrossing(line1: List[Double], line2: List[Double]): Option[Condition] =
    (line1.head, line2.head, line1.drop(1).head, line2.drop(1).head) match
      case (l1c, l2c, l1p, l2p) if l1c >= l2c && l1p < l2p => Some(Condition.CrossingUp)
      case (l1c, l2c, l1p, l2p) if l1c <= l2c && l1p > l2p => Some(Condition.CrossingDown)
      case _                                               => None
}
