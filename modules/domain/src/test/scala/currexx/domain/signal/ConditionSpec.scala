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
        val line = List(5.0, 1.0, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.ThresholdCrossing(4.0, 5.0, Direction.Upward, Boundary.Upper))
      }

      "return BelowThreshold when current value is below min" in {
        val line = List(0.5, 1.1, 1.0, 1.0, 1.0)

        Condition.thresholdCrossing(line, 1.0, 4.0) mustBe Some(Condition.ThresholdCrossing(1.0, 0.5, Direction.Downward, Boundary.Lower))
      }

      "return None when value is within limits" in {
        val line = List(3.0, 3.5, 1.0, 1.0, 1.0)

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
            Condition.thresholdCrossing(List(current, previous), 20.0, 80.0) mustBe
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
            Condition.thresholdCrossing(List(current, previous), 20.0, 80.0) mustBe
              Some(Condition.ThresholdCrossing(BigDecimal.valueOf(threshold), BigDecimal.valueOf(current), direction, boundary))
          }
        }
      }

      "return None when both values remain in the same zone" in {
        val moves = List((90.0, 85.0), (10.0, 15.0), (50.0, 55.0), (90.0, 80.0), (10.0, 20.0), (80.0, 80.0), (20.0, 20.0))

        moves.foreach { (previous, current) =>
          withClue(s"$previous -> $current: ") {
            Condition.thresholdCrossing(List(current, previous), 20.0, 80.0) mustBe None
          }
        }
      }

      "return None without two values" in {
        Condition.thresholdCrossing(Nil, 20.0, 80.0) mustBe None
        Condition.thresholdCrossing(List(10.0), 20.0, 80.0) mustBe None
      }
    }

    "linesCrossing" should {
      "return LinesCrossing(Downward) / CrossingDown when line 1 (slow) crosses line 2 (fast) from above" in {
        val line1 = List(1.0, 3.0, 3.0, 3.0, 3.0)
        val line2 = List(3.0, 1.0, 1.0, 1.0, 1.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.LinesCrossing(Direction.Downward))
      }

      "return LinesCrossing(Upward) / CrossingUp when line 1 (slow) crosses line 2 (fast) from below" in {
        val line1 = List(3.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = List(2.0, 2.0, 2.0, 2.0, 2.0)

        Condition.linesCrossing(line1, line2) mustBe Some(Condition.LinesCrossing(Direction.Upward))
      }

      "return None when lines do not intersect" in {
        val line1 = List(2.0, 1.0, 1.0, 1.0, 1.0)
        val line2 = List(3.0, 3.0, 3.0, 3.0, 3.0)

        Condition.linesCrossing(line1, line2) mustBe None
      }
    }

    "trendDirectionChange" should {
      "return TrendDirectionChange when trend changes from Upward to Downward" in {
        val line = List(1.1422, 1.1522, 1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe Some(Condition.TrendDirectionChange(Direction.Upward, Direction.Downward, Some(6)))
      }

      "return None when trend doesn't change" in {
        val line = List(1.1464, 1.1346, 1.1239, 1.1134, 1.1109, 1.1177, 1.1339, 1.1443, 1.1417, 1.1382, 1.1393)

        Condition.trendDirectionChange(line) mustBe None
      }
    }

    "barrierCrossing" should {
      val upperBarrier = List(5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0, 5.0)
      val lowerBarrier = List(1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0, 1.0)

      "return UpperBandCrossing when line crosses upper barrier" in {
        val line1 = List(4.0, 6.0, 6.0, 6.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe Some(Condition.UpperBandCrossing(Direction.Downward))

        val line2 = List(6.0, 4.0, 4.0, 4.0)
        Condition.bandCrossing(line2, upperBarrier, lowerBarrier) mustBe Some(Condition.UpperBandCrossing(Direction.Upward))
      }

      "return LowerBandCrossing when line crosses lower barrier" in {
        val line1 = List(0.0, 2.0, 2.0, 2.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe Some(Condition.LowerBandCrossing(Direction.Downward))

        val line2 = List(2.0, 0.0, 0.0, 0.0)
        Condition.bandCrossing(line2, upperBarrier, lowerBarrier) mustBe Some(Condition.LowerBandCrossing(Direction.Upward))
      }

      "return None when line doesn't cross boundaries" in {
        val line1 = List(4.0, 3.0, 3.0, 3.0)
        Condition.bandCrossing(line1, upperBarrier, lowerBarrier) mustBe None
      }
    }

    "priceCrossedLine" should {
      val lineRole = ValueRole.ChannelMiddleBand

      "return PriceCrossedLine with Upward direction when price crosses above line" in {
        val priceLine = List(5.0, 3.0, 3.0, 3.0)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Upward)
        )
      }

      "return PriceCrossedLine with Downward direction when price crosses below line" in {
        val priceLine = List(3.0, 5.0, 5.0, 5.0)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Downward)
        )
      }

      "return None when price doesn't cross the line" in {
        val priceLine = List(5.0, 5.5, 5.5, 5.5)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe None
      }

      "return None when price touches but doesn't cross" in {
        val priceLine = List(4.0, 4.0, 4.0, 4.0)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe None
      }

      "return None when insufficient data" in {
        val priceLine1 = List(5.0)
        val otherLine1 = List(4.0, 4.0)

        Condition.priceCrossedLine(priceLine1, otherLine1, lineRole) mustBe None

        val priceLine2 = List(5.0, 3.0)
        val otherLine2 = List(4.0)

        Condition.priceCrossedLine(priceLine2, otherLine2, lineRole) mustBe None
      }

      "handle equal values correctly for upward crossing" in {
        val priceLine = List(4.0, 3.0, 3.0, 3.0)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Upward)
        )
      }

      "handle equal values correctly for downward crossing" in {
        val priceLine = List(4.0, 5.0, 5.0, 5.0)
        val otherLine = List(4.0, 4.0, 4.0, 4.0)

        Condition.priceCrossedLine(priceLine, otherLine, lineRole) mustBe Some(
          Condition.PriceCrossedLine(lineRole, Direction.Downward)
        )
      }
    }
  }
}
