package currexx.domain.signal

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ConditionSpec extends AnyWordSpec with Matchers {

  "A Condition" when {
    "working with json codecs" should {
      "decode lines-crossing condition from json" in {
        val condition: Condition = Condition.LinesCrossing(Direction.Upward)
        val json                 = """{"direction":"upward","kind":"lines-crossing"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode above-threshold condition from json" in {
        val condition: Condition = Condition.ThresholdCrossing(BigDecimal(10), BigDecimal(20), Direction.Upward, Boundary.Upper)
        val json                 = """{"threshold":10,"value":20,"direction":"upward","boundary":"upper","kind":"threshold-crossing"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }

      "decode trend-direction-change condition from json" in {
        val condition: Condition = Condition.TrendDirectionChange(Direction.Upward, Direction.Downward)
        val json                 = """{"from":"upward","to":"downward","previousTrendLength":null,"kind":"trend-direction-change"}"""

        condition.asJson.noSpaces mustBe json
        decode[Condition](json) mustBe Right(condition)
      }
    }

    "thresholdCrossing" should {
      "return AboveThreshold when current value is above max" in {
        val line = Array(5.0, 1.0, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.ThresholdCrossing(4.0, 5.0, Direction.Upward, Boundary.Upper))
      }

      "return BelowThreshold when current value is below min" in {
        val line = Array(0.5, 1.1, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.ThresholdCrossing(1.0, 0.5, Direction.Downward, Boundary.Lower))
      }

      "return None when value is within limits" in {
        val line = Array(3.0, 3.5, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe None
      }

      "report the destination boundary when a move crosses both thresholds" in {
        val crossings = List(
          (90.0, 10.0, 20.0, Direction.Downward, Boundary.Lower),
          (10.0, 90.0, 80.0, Direction.Upward, Boundary.Upper),
          (90.0, 20.0, 20.0, Direction.Downward, Boundary.Lower),
          (10.0, 80.0, 80.0, Direction.Upward, Boundary.Upper),
          (80.0, 10.0, 20.0, Direction.Downward, Boundary.Lower),
          (20.0, 90.0, 80.0, Direction.Upward, Boundary.Upper),
          (80.0, 20.0, 20.0, Direction.Downward, Boundary.Lower),
          (20.0, 80.0, 80.0, Direction.Upward, Boundary.Upper)
        )

        crossings.foreach { (previous, current, threshold, direction, boundary) =>
          withClue(s"$previous -> $current: ") {
            Condition.thresholdCrossing(Array(current, previous), 20.0, 80.0) mustBe
              Some(Condition.ThresholdCrossing(BigDecimal.valueOf(threshold), BigDecimal.valueOf(current), direction, boundary))
          }
        }
      }

      "include the boundaries in the extreme zones and detect departures into neutral" in {
        val crossings = List(
          (50.0, 80.0, 80.0, Direction.Upward, Boundary.Upper),
          (50.0, 20.0, 20.0, Direction.Downward, Boundary.Lower),
          (80.0, 79.0, 80.0, Direction.Downward, Boundary.Upper),
          (20.0, 21.0, 20.0, Direction.Upward, Boundary.Lower),
          (90.0, 50.0, 80.0, Direction.Downward, Boundary.Upper),
          (10.0, 50.0, 20.0, Direction.Upward, Boundary.Lower)
        )

        crossings.foreach { (previous, current, threshold, direction, boundary) =>
          withClue(s"$previous -> $current: ") {
            Condition.thresholdCrossing(Array(current, previous), 20.0, 80.0) mustBe
              Some(Condition.ThresholdCrossing(BigDecimal.valueOf(threshold), BigDecimal.valueOf(current), direction, boundary))
          }
        }
      }

      "return None when both values remain in the same zone" in {
        val moves = List((90.0, 85.0), (10.0, 15.0), (50.0, 55.0), (90.0, 80.0), (10.0, 20.0), (80.0, 80.0), (20.0, 20.0))

        moves.foreach { (previous, current) =>
          withClue(s"$previous -> $current: ") {
            Condition.thresholdCrossing(Array(current, previous), 20.0, 80.0) mustBe None
          }
        }
      }

      "return None without two values" in {
        Condition.thresholdCrossing(Array.empty[Double], 20.0, 80.0) mustBe None
        Condition.thresholdCrossing(Array(10.0), 20.0, 80.0) mustBe None
      }

      "ignore undefined comparisons and treat signed zero as equality" in {
        Condition.thresholdCrossing(Array(Double.NaN, 10.0), 20.0, 80.0) mustBe None
        Condition.thresholdCrossing(Array(90.0, Double.NaN), 20.0, 80.0) mustBe None
        Condition.thresholdCrossing(Array(-0.0, 0.0), 0.0, 0.0) mustBe None
      }
    }

    "linesCrossing" should {
      "return LinesCrossing(Downward) / CrossingDown when line 1 (slow) crosses line 2 (fast) from above" in {
        val line1 = Array(1.0, 3.0, 3.0, 3.0, 3.0)
        val line2 = Array(3.0, 1.0, 1.0, 1.0, 1.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.LinesCrossing(Direction.Downward))
      }

      "return LinesCrossing(Upward) / CrossingUp when line 1 (slow) crosses line 2 (fast) from below" in {
        val line1 = Array(3.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = Array(2.0, 2.0, 2.0, 2.0, 2.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.LinesCrossing(Direction.Upward))
      }

      "return None when lines do not intersect" in {
        val line1 = Array(2.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = Array(3.0, 3.0, 3.0, 3.0, 3.0)

        Condition.linesCrossing(line1, line2) mustBe None
      }

      "include the current equality but require a strict previous difference" in {
        Condition.linesCrossing(Array(0.0, -1.0), Array(-0.0, 0.0)) mustBe Some(Condition.LinesCrossing(Direction.Upward))
        Condition.linesCrossing(Array(0.0, 1.0), Array(-0.0, 0.0)) mustBe Some(Condition.LinesCrossing(Direction.Downward))
        Condition.linesCrossing(Array(1.0, 0.0), Array(0.0, -0.0)) mustBe None
      }

      "handle insufficient lengths and non-finite comparisons" in {
        Condition.linesCrossing(Array.empty[Double], Array(0.0, 0.0)) mustBe None
        Condition.linesCrossing(Array(1.0, -1.0), Array(0.0)) mustBe None
        Condition.linesCrossing(Array(Double.NaN, -1.0), Array(0.0, 0.0)) mustBe None
        Condition.linesCrossing(Array(1.0, -1.0), Array(0.0, Double.NaN)) mustBe None
        Condition.linesCrossing(Array(Double.PositiveInfinity, Double.NegativeInfinity), Array(0.0, 0.0)) mustBe
          Some(Condition.LinesCrossing(Direction.Upward))
      }
    }

    "trendDirectionChange" should {
      "return TrendDirectionChange when trend changes from Upward to Downward" in {
        val line = Array(1.1422, 1.1522, 1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(6)))
      }

      "return None when trend doesn't change" in {
        val line = Array(1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe None
      }

      "confirm peaks and troughs using the requested lookback" in {
        val peak = Array(1.0, 2.0, 5.0, 4.0, 3.0, 2.0, 1.0)
        Condition.trendDirectionChange(peak, lookback = 2) mustBe
          Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(5)))
        Condition.trendDirectionChange(peak.map(-_), lookback = 2) mustBe
          Some(Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(5)))
        Condition.trendDirectionChange(peak.take(4), lookback = 2) mustBe None
        Condition.trendDirectionChange(Array.empty[Double]) mustBe None
      }

      "measure trend duration over the complete history and stop at a flat or undefined segment" in {
        val history = Array(97.0, 99.0) ++ (98 to 1 by -1).map(_.toDouble)
        Condition.trendDirectionChange(history) mustBe
          Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(99)))
        Condition.trendDirectionChange(Array(4.0, 6.0, 5.0, 4.0, 4.0, 3.0)) mustBe
          Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(3)))
        Condition.trendDirectionChange(Array(4.0, 6.0, 5.0, Double.NaN, 3.0)) mustBe
          Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(2)))
        Condition.trendDirectionChange(Array(4.0, Double.NaN, 5.0)) mustBe None
      }
    }

    "barrierCrossing" should {
      val upperBarrier = Array(5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0)
      val lowerBarrier = Array(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)

      "return UpperBandCrossing when line crosses upper barrier" in {
        val line1 = Array(4.0, 6.0, 6.0, 6.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe Some(Condition.UpperBandCrossing(Direction.Downward))

        val line2 = Array(6.0, 4.0, 4.0, 4.0)
        Condition.bandCrossing(line2, upperBarrier, lowerBarrier) mustBe Some(Condition.UpperBandCrossing(Direction.Upward))
      }

      "return LowerBandCrossing when line crosses lower barrier" in {
        val line1 = Array(0.0, 2.0, 2.0, 2.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe Some(Condition.LowerBandCrossing(Direction.Downward))

        val line2 = Array(2.0, 0.0, 0.0, 0.0)
        Condition.bandCrossing(line2, upperBarrier, lowerBarrier) mustBe Some(Condition.LowerBandCrossing(Direction.Upward))
      }

      "return None when line doesn't cross boundaries" in {
        val line1 = Array(4.0, 3.0, 3.0, 3.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe None
      }

      "prioritize the upper band when both boundaries are crossed" in {
        Condition.bandCrossing(Array(2.0, -2.0), Array(1.0, 1.0), Array(-1.0, -1.0)) mustBe
          Some(Condition.UpperBandCrossing(Direction.Upward))
        Condition.bandCrossing(Array(-2.0, 2.0), Array(1.0, 1.0), Array(-1.0, -1.0)) mustBe
          Some(Condition.UpperBandCrossing(Direction.Downward))
      }

      "check the lower band when the upper band has insufficient history" in {
        Condition.bandCrossing(Array(0.0, -2.0), Array(1.0), Array(-1.0, -1.0)) mustBe
          Some(Condition.LowerBandCrossing(Direction.Upward))
        Condition.bandCrossing(Array(2.0), Array(1.0, 1.0), Array(-1.0, -1.0)) mustBe None
      }
    }

    "volatilityRegimeChange" should {
      "report an initial regime with two observations on either line" in {
        Condition.volatilityRegimeChange(Array(2.0, 2.0), Array(1.0, 1.0)) mustBe
          Some(Condition.VolatilityRegimeChange(None, VolatilityRegime.High))
        Condition.volatilityRegimeChange(Array(1.0, 1.0), Array(1.0, 1.0)) mustBe
          Some(Condition.VolatilityRegimeChange(None, VolatilityRegime.Low))
        Condition.volatilityRegimeChange(Array(2.0, 2.0, 2.0), Array(1.0, 1.0)) mustBe
          Some(Condition.VolatilityRegimeChange(None, VolatilityRegime.High))
        Condition.volatilityRegimeChange(Array(2.0, 2.0), Array(1.0, 1.0, 1.0)) mustBe
          Some(Condition.VolatilityRegimeChange(None, VolatilityRegime.High))
      }

      "report only changed regimes when both lines have at least three observations" in {
        val smoothed = Array(1.0, 1.0, 1.0)
        Condition.volatilityRegimeChange(Array(2.0, 0.0, 0.0), smoothed) mustBe
          Some(Condition.VolatilityRegimeChange(Some(VolatilityRegime.Low), VolatilityRegime.High))
        Condition.volatilityRegimeChange(Array(0.0, 2.0, 2.0), smoothed) mustBe
          Some(Condition.VolatilityRegimeChange(Some(VolatilityRegime.High), VolatilityRegime.Low))
        Condition.volatilityRegimeChange(Array(2.0, 2.0, 2.0), smoothed) mustBe None
        Condition.volatilityRegimeChange(Array(1.0, 1.0, 1.0), smoothed) mustBe None
      }

      "require two values on each line and treat equality or undefined comparisons as low volatility" in {
        val smoothed = Array(1.0, 1.0, 1.0)
        Condition.volatilityRegimeChange(Array.empty[Double], smoothed) mustBe None
        Condition.volatilityRegimeChange(smoothed, Array(1.0)) mustBe None
        Condition.volatilityRegimeChange(Array(1.0, 2.0, 2.0), smoothed) mustBe
          Some(Condition.VolatilityRegimeChange(Some(VolatilityRegime.High), VolatilityRegime.Low))
        Condition.volatilityRegimeChange(Array(Double.NaN, 2.0, 2.0), smoothed) mustBe
          Some(Condition.VolatilityRegimeChange(Some(VolatilityRegime.High), VolatilityRegime.Low))
      }
    }

    "evaluating array inputs" should {
      "leave every input value unchanged" in {
        val line   = Array(1.0, 3.0, 2.0, -0.0, Double.NaN)
        val other  = Array(2.0, 2.0, 2.0, 0.0, 0.0)
        val upper  = Array(3.0, 3.0)
        val lower  = Array(0.0, 0.0)
        val inputs = List(line, other, upper, lower)
        val before = inputs.map(_.map(java.lang.Double.doubleToRawLongBits).toList)

        Condition.linesCrossing(line, other)
        Condition.priceCrossedLine(line, other, ValueRole.Price)
        Condition.bandCrossing(line, upper, lower)
        Condition.thresholdCrossing(line, 0.0, 2.0)
        Condition.trendDirectionChange(line)
        Condition.volatilityRegimeChange(line, other)

        inputs.map(_.map(java.lang.Double.doubleToRawLongBits).toList) mustBe before
      }
    }

    "priceCrossedLine" should {
      val lineRole = ValueRole.ChannelMiddleBand

      "return PriceCrossedLine with Upward direction when price crosses above line" in {
        val priceLine = Array(5.0, 3.0, 3.0, 3.0)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Upward)
        )
      }

      "return PriceCrossedLine with Downward direction when price crosses below line" in {
        val priceLine = Array(3.0, 5.0, 5.0, 5.0)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Downward)
        )
      }

      "return None when price doesn't cross the line" in {
        val priceLine = Array(5.0, 5.5, 5.5, 5.5)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe None
      }

      "return None when price touches but doesn't cross" in {
        val priceLine = Array(4.0, 4.0, 4.0, 4.0)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe None
      }

      "return None when insufficient data" in {
        val priceLine1 = Array(5.0)
        val otherLine1 = Array(4.0, 4.0)

        Condition.priceCrossedLine(priceLine1, otherLine1, lineRole) mustBe None

        val priceLine2 = Array(5.0, 3.0)
        val otherLine2 = Array(4.0)

        Condition.priceCrossedLine(priceLine2, otherLine2, lineRole) mustBe None
      }

      "handle equal values correctly for upward crossing" in {
        val priceLine = Array(4.0, 3.0, 3.0, 3.0)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Upward)
        )
      }

      "handle equal values correctly for downward crossing" in {
        val priceLine = Array(4.0, 5.0, 5.0, 5.0)
        val otherLine = Array(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Downward)
        )
      }
    }
  }
}
