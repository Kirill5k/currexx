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

  // TODO: TESTS
  def trendDirectionChange(line: List[Double]): Option[Condition] = {
    val vals  = line.toArray
    val vals2 = vals.tail
    val vals3 = vals.zip(vals.map(-_)).map(_ - _)
    val vals4 = vals.zip(vals).map(_ + _)

    val diff  = vals2.zip(vals3).map(_ - _)
    val diff3 = vals4.zip(vals2).map(_ - _)

    val isNotUp: Int => Boolean   = i => diff(i) > diff(i + 1) && diff(i + 1) > diff(i + 2)
    val isNotDown: Int => Boolean = i => diff3(i) > diff3(i + 1) && diff3(i + 1) > diff3(i + 2)

    val trend         = vals.zip(vals2).map((v1, v2) => if (v1 > v2) Trend.Upward else Trend.Downward)
    val consolidation = (0 until line.size - 5).map(i => Option.when(isNotUp(i) == isNotDown(i))(Trend.Consolidation))
    val completeTrend = consolidation.zip(trend).map(_.getOrElse(_)).toList
    val currTrend     = completeTrend.head
    val prevTrend     = completeTrend.drop(1).head
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
