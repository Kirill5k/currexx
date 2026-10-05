package currexx.calculations

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StatisticsSpec extends AnyWordSpec with Matchers {

  "A Statistics" should {
    "calculate standard deviation for the specified period" in {
      val values = List(10.0, 8.0, 6.0, 4.0, 2.0) // Latest to earliest
      val result = Statistics.standardDeviation(values, 3)

      // Expected calculations (chronological order: 2, 4, 6, 8, 10):
      // Window 1: [2] -> size < 3, stdDev = 0.0
      // Window 2: [2, 4] -> size < 3, stdDev = 0.0
      // Window 3: [2, 4, 6] -> mean = 4, sample stdDev = sqrt(((2-4)^2 + (4-4)^2 + (6-4)^2) / 2) = sqrt(8/2) = 2.0
      // Window 4: [4, 6, 8] -> mean = 6, sample stdDev = sqrt(((4-6)^2 + (6-6)^2 + (8-6)^2) / 2) = sqrt(8/2) = 2.0
      // Window 5: [6, 8, 10] -> mean = 8, sample stdDev = sqrt(((6-8)^2 + (8-8)^2 + (10-8)^2) / 2) = sqrt(8/2) = 2.0
      // Result reversed: [2.0, 2.0, 2.0, 0.0, 0.0]

      result must have size 5
      result(0) mustBe 2.0 +- 0.0001
      result(1) mustBe 2.0 +- 0.0001
      result(2) mustBe 2.0 +- 0.0001
      result(3) mustBe 0.0
      result(4) mustBe 0.0
    }

    "handle window size of 1" in {
      val values = List(5.0, 4.0, 3.0, 2.0, 1.0)
      val result = Statistics.standardDeviation(values, 1)

      // With window size 1, variance is 0 (no variation in a single point)
      result must have size 5
      result.foreach(_ mustBe 0.0)
    }

    "handle window size equal to data length" in {
      val values = List(5.0, 3.0, 1.0) // Latest to earliest
      val result = Statistics.standardDeviation(values, 3)

      // Chronological: [1, 3, 5]
      // Mean = 3, sample stdDev = sqrt(((1-3)^2 + (3-3)^2 + (5-3)^2) / 2) = sqrt(8/2) = 2.0
      result must have size 3
      result(0) mustBe 2.0 +- 0.0001
      result(1) mustBe 0.0
      result(2) mustBe 0.0
    }

    "handle window size larger than data length" in {
      val values = List(4.0, 3.0, 2.0, 1.0)
      val result = Statistics.standardDeviation(values, 5)

      // Window never reaches size 5, so all results should be 0.0
      result must have size 4
      result.foreach(_ mustBe 0.0)
    }

    "calculate correct sample standard deviation" in {
      val values = List(6.0, 4.0, 2.0) // Latest to earliest
      val result = Statistics.standardDeviation(values, 3)

      // Chronological: [2, 4, 6]
      // Mean = 4
      // Sample variance = ((2-4)^2 + (4-4)^2 + (6-4)^2) / (3-1) = (4 + 0 + 4) / 2 = 4
      // Sample stdDev = sqrt(4) = 2.0
      result must have size 3
      result(0) mustBe 2.0 +- 0.0001
    }

    "handle identical values" in {
      val values = List(5.0, 5.0, 5.0, 5.0)
      val result = Statistics.standardDeviation(values, 2)

      // All values are the same, so stdDev should be 0
      result must have size 4
      result(0) mustBe 0.0
      result(1) mustBe 0.0
      result(2) mustBe 0.0
      result(3) mustBe 0.0
    }

    "handle single value" in {
      val values = List(42.0)
      val result = Statistics.standardDeviation(values, 1)

      result must have size 1
      result(0) mustBe 0.0
    }

    "handle negative values" in {
      val values = List(2.0, -2.0, -6.0) // Latest to earliest
      val result = Statistics.standardDeviation(values, 3)

      // Chronological: [-6, -2, 2]
      // Mean = -2
      // Sample variance = ((-6-(-2))^2 + (-2-(-2))^2 + (2-(-2))^2) / 2 = (16 + 0 + 16) / 2 = 16
      // Sample stdDev = sqrt(16) = 4.0
      result must have size 3
      result(0) mustBe 4.0 +- 0.0001
    }

    "handle decimal values with precision" in {
      val values = List(1.5, 2.5, 3.5, 4.5) // Latest to earliest
      val result = Statistics.standardDeviation(values, 2)

      // Rolling windows of size 2:
      // [3.5, 4.5]: mean = 4.0, sample stdDev = sqrt(((3.5-4)^2 + (4.5-4)^2) / 1) = sqrt(0.5) ≈ 0.7071
      // [2.5, 3.5]: mean = 3.0, sample stdDev = sqrt(0.5) ≈ 0.7071
      // [1.5, 2.5]: mean = 2.0, sample stdDev = sqrt(0.5) ≈ 0.7071
      result must have size 4
      result(0) mustBe 0.7071 +- 0.0001
      result(1) mustBe 0.7071 +- 0.0001
      result(2) mustBe 0.7071 +- 0.0001
      result(3) mustBe 0.0
    }

    "handle empty list" in {
      val values = List.empty[Double]
      val result = Statistics.standardDeviation(values, 3)

      result must be(empty)
    }

    "calculate exact array sample deviations over 100 bars" in {
      val input = Array.tabulate(100)(_.toDouble)
      Statistics.standardDeviation(input, 3).toList mustBe List.fill(98)(1.0) ++ List(0.0, 0.0)
      Statistics.standardDeviation(input, 1).toList mustBe List.fill(100)(0.0)
      Statistics.standardDeviation(input, 100).toList mustBe math.sqrt(100.0 * 101.0 / 12.0) :: List.fill(99)(0.0)
      Statistics.standardDeviation(input, 101).toList mustBe List.fill(100)(0.0)
      Statistics.standardDeviation(Array.emptyDoubleArray, 3).toList mustBe Nil
      Statistics.standardDeviation(Array(2.0), 1).toList mustBe List(0.0)
    }

    "keep deviation input arrays and outputs independent" in {
      val input  = Array(5.0, 3.0, 1.0)
      val first  = Statistics.standardDeviation(input, 3)
      val second = Statistics.standardDeviation(input, 3)
      first.toList mustBe List(2.0, 0.0, 0.0)
      first(0) = -1.0
      input.toList mustBe List(5.0, 3.0, 1.0)
      second.toList mustBe List(2.0, 0.0, 0.0)
    }

    "retain nonfinite and signed-zero arithmetic in array windows" in {
      for (value <- List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity)) {
        val result = Statistics.standardDeviation(Array(value, 3.0, 2.0, 1.0), 2)
        result.head.isNaN mustBe true
        result.tail.toList mustBe List(math.sqrt(0.5), math.sqrt(0.5), 0.0)
        Statistics.standardDeviation(Array(value), 1).toList mustBe List(0.0)
      }
      val zeroResult = Statistics.standardDeviation(Array(-0.0, 0.0, -0.0, 0.0), 2)
      zeroResult.map(java.lang.Double.doubleToRawLongBits).toList mustBe List.fill(4)(0L)
    }

    "require a positive standard-deviation period" in {
      for (period <- List(0, -1))
        intercept[IllegalArgumentException](Statistics.standardDeviation(Array(1.0), period))
      succeed
    }
  }
}
