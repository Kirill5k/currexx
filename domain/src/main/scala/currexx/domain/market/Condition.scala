package currexx.domain.market

import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object Trend extends EnumType[Trend](() => Trend.values, _.print)
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

    val diff                        = previous.zip(current.map(_ * 2)).map(_ - _)
    val isGrowing: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isDeclining: Int => Boolean = i => diff(i) < diff(i + 1) && diff(i + 1) < diff(i + 2)

    val trend         = current.zip(previous).map((c, p) => if (c > p) Trend.Upward else Trend.Downward)
    val completeTrend = (0 until line.size - 3).toList.map(i => if (!isGrowing(i) && !isDeclining(i)) Trend.Consolidation else trend(i))
    val currTrend     = completeTrend.head
    val prevTrend     = completeTrend.tail.head
    Option
      .when(currTrend != prevTrend)(
        Condition.TrendDirectionChange(prevTrend, currTrend, Some(completeTrend.tail.takeWhile(_ == prevTrend).size))
      )
  }

  def thresholdCrossing(line: List[Double], min: Double, max: Double): Option[Condition] =
    if (max < line.head) Some(Condition.AboveThreshold(max, line.head))
    else if (min > line.head) Some(Condition.BelowThreshold(min, line.head))
    else None

  def linesCrossing(line1: List[Double], line2: List[Double]): Option[Condition] =
    (line1.head, line2.head, line1.drop(1).head, line2.drop(1).head) match
      case (l1c, l2c, l1p, l2p) if l1c > l2c && l1p < l2p => Some(Condition.CrossingUp)
      case (l1c, l2c, l1p, l2p) if l1c < l2c && l1p > l2p => Some(Condition.CrossingDown)
      case _                                              => None
}
