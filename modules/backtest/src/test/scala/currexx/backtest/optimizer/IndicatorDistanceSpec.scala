package currexx.backtest.optimizer

import currexx.algorithms.operators.species.Distance
import currexx.backtest.TestStrategy
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, MovingAverage, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

class IndicatorDistanceSpec extends IOWordSpec {
  private val close = ValueSource.Close

  private def trend(vt: VT): Indicator = Indicator.TrendChangeDetection(close, vt)

  private def distance(
      template: Indicator,
      fixed: Set[Indicator] = Set.empty,
      conditions: List[Rule.Condition] = Nil
  ): Distance[Indicator] = {
    val strategy = TestStrategy(template, TradeStrategy(conditions.map(Rule(TradeAction.OpenLong, _)), Nil))
    val space    = IndicatorSearchSpace.forStrategy(strategy, fixed).fold(error => fail(error.getMessage), identity)
    IndicatorDistance.make(space)
  }

  extension (metric: Distance[Indicator])
    private def valueBetween(a: Indicator, b: Indicator): Double =
      metric.between(a, b).fold(error => fail(s"Expected a valid distance: ${error.getMessage}"), identity)

  "IndicatorDistance" should {
    "be zero for identical candidates and symmetric for distinct candidates" in {
      val a = trend(VT.JMA(13, -10, 2))
      val b = trend(VT.JMA(65, 35, 6))
      val d = distance(a)

      d.valueBetween(a, a) mustBe 0.0
      d.valueBetween(a, b) mustBe d.valueBetween(b, a)
      d.valueBetween(a, b) must be > 0.0
    }

    "measure equal lookback proportions equally throughout the search range" in {
      val d = distance(trend(VT.SMA(5)))
      d.valueBetween(trend(VT.SMA(5)), trend(VT.SMA(10))) mustBe
        (d.valueBetween(trend(VT.SMA(50)), trend(VT.SMA(100))) +- 1e-12)
      d.valueBetween(trend(VT.SMA(5)), trend(VT.SMA(100))) mustBe (1.0 +- 1e-12)
    }

    "use linear phase, logarithmic power and RMS across the three JMA genes" in {
      val a = trend(VT.JMA(5, -100, 1))
      val d = distance(a)
      d.valueBetween(a, trend(VT.JMA(5, 100, 1))) mustBe (1.0 / math.sqrt(3.0) +- 1e-12)
      d.valueBetween(a, trend(VT.JMA(5, -100, 10))) mustBe (1.0 / math.sqrt(3.0) +- 1e-12)
      d.valueBetween(trend(VT.JMA(5, -50, 1)), trend(VT.JMA(5, 0, 1))) mustBe
        (d.valueBetween(trend(VT.JMA(5, 0, 1)), trend(VT.JMA(5, 50, 1))) +- 1e-12)
    }

    "scale every known transformation using its own gene bounds" in {
      val endpoints = List(
        VT.StandardDeviation(5)                 -> VT.StandardDeviation(100),
        VT.Kalman(0.01, 0.01)                   -> VT.Kalman(0.5, 1.0),
        VT.KalmanVelocity(0.01, 0.01)           -> VT.KalmanVelocity(0.5, 1.0),
        VT.ATR(5)                               -> VT.ATR(50),
        VT.RSX(5)                               -> VT.RSX(50),
        VT.JRSX(5)                              -> VT.JRSX(50),
        VT.WMA(5)                               -> VT.WMA(100),
        VT.SMA(5)                               -> VT.SMA(100),
        VT.EMA(5)                               -> VT.EMA(100),
        VT.HMA(5)                               -> VT.HMA(100),
        VT.NMA(5, 5, 0.5, MovingAverage.Simple) -> VT.NMA(50, 50, 4.0, MovingAverage.Simple),
        VT.JMA(5, -100, 1)                      -> VT.JMA(100, 100, 10),
        VT.STOCH(5)                             -> VT.STOCH(50),
        VT.ADX(7)                               -> VT.ADX(50),
        VT.WilliamsR(5)                         -> VT.WilliamsR(50),
        VT.CCI(10)                              -> VT.CCI(50),
        VT.IchimokuKijunSen(9)                  -> VT.IchimokuKijunSen(52),
        VT.ParabolicSAR(0.01, 0.1, 0.01)        -> VT.ParabolicSAR(0.05, 0.4, 0.05),
        VT.CMF(10)                              -> VT.CMF(40),
        VT.sequenced(VT.SMA(5), VT.ATR(5))      -> VT.sequenced(VT.SMA(100), VT.ATR(50))
      )

      endpoints.foreach { case (lo, hi) =>
        withClue(s"$lo to $hi: ") {
          distance(trend(lo)).valueBetween(trend(lo), trend(hi)) mustBe (1.0 +- 1e-12)
        }
      }
      succeed
    }

    "include every coordinate of transformations with multiple parameters" in {
      val cases = List(
        (VT.Kalman(0.01, 0.01), VT.Kalman(0.5, 0.01), 2),
        (VT.Kalman(0.01, 0.01), VT.Kalman(0.01, 1.0), 2),
        (VT.KalmanVelocity(0.01, 0.01), VT.KalmanVelocity(0.5, 0.01), 2),
        (VT.KalmanVelocity(0.01, 0.01), VT.KalmanVelocity(0.01, 1.0), 2),
        (VT.NMA(5, 5, 0.5, MovingAverage.Simple), VT.NMA(50, 5, 0.5, MovingAverage.Simple), 3),
        (VT.NMA(5, 5, 0.5, MovingAverage.Simple), VT.NMA(5, 50, 0.5, MovingAverage.Simple), 3),
        (VT.NMA(5, 5, 0.5, MovingAverage.Simple), VT.NMA(5, 5, 4.0, MovingAverage.Simple), 3),
        (VT.ParabolicSAR(0.01, 0.1, 0.01), VT.ParabolicSAR(0.05, 0.1, 0.01), 3),
        (VT.ParabolicSAR(0.01, 0.1, 0.01), VT.ParabolicSAR(0.01, 0.4, 0.01), 3),
        (VT.ParabolicSAR(0.01, 0.1, 0.01), VT.ParabolicSAR(0.01, 0.1, 0.05), 3)
      )

      cases.foreach { case (a, b, dimensions) =>
        withClue(s"$a to $b: ") {
          distance(trend(a)).valueBetween(trend(a), trend(b)) mustBe (1.0 / math.sqrt(dimensions.toDouble) +- 1e-12)
        }
      }
      succeed
    }

    "include channel, volatility, crossover, price line and active tracked-value genes" in {
      val cases = List(
        (Indicator.LinesCrossing(close, VT.SMA(5), VT.EMA(5)), Indicator.LinesCrossing(close, VT.SMA(100), VT.EMA(5)), 2),
        (Indicator.LinesCrossing(close, VT.SMA(5), VT.EMA(5)), Indicator.LinesCrossing(close, VT.SMA(5), VT.EMA(100)), 2),
        (Indicator.KeltnerChannel(close, VT.SMA(5), 5, 0.5), Indicator.KeltnerChannel(close, VT.SMA(100), 5, 0.5), 3),
        (Indicator.KeltnerChannel(close, VT.SMA(5), 5, 0.5), Indicator.KeltnerChannel(close, VT.SMA(5), 50, 0.5), 3),
        (Indicator.KeltnerChannel(close, VT.SMA(5), 5, 0.5), Indicator.KeltnerChannel(close, VT.SMA(5), 5, 5.0), 3),
        (Indicator.BollingerBands(close, VT.SMA(5), 5, 1.0), Indicator.BollingerBands(close, VT.SMA(100), 5, 1.0), 3),
        (Indicator.BollingerBands(close, VT.SMA(5), 5, 1.0), Indicator.BollingerBands(close, VT.SMA(5), 50, 1.0), 3),
        (Indicator.BollingerBands(close, VT.SMA(5), 5, 1.0), Indicator.BollingerBands(close, VT.SMA(5), 5, 4.0), 3),
        (Indicator.VolatilityRegimeDetection(5, VT.SMA(5)), Indicator.VolatilityRegimeDetection(50, VT.SMA(5)), 2),
        (Indicator.VolatilityRegimeDetection(5, VT.SMA(5)), Indicator.VolatilityRegimeDetection(5, VT.SMA(100)), 2),
        (
          Indicator.PriceLineCrossing(close, ValueRole.Price, VT.SMA(5)),
          Indicator.PriceLineCrossing(close, ValueRole.Price, VT.SMA(100)),
          1
        ),
        (
          Indicator.ValueTracking(ValueRole.Volatility, close, VT.ATR(5)),
          Indicator.ValueTracking(ValueRole.Volatility, close, VT.ATR(50)),
          1
        )
      )

      cases.foreach { case (a, b, dimensions) =>
        withClue(s"$a to $b: ") {
          val d = distance(a, conditions = List(Rule.Condition.ValueIs(ValueRole.Volatility, Rule.Operator.GreaterThan, 1)))
          d.valueBetween(a, b) mustBe (1.0 / math.sqrt(dimensions.toDouble) +- 1e-12)
        }
      }
      succeed
    }

    "use each oscillator's upper and lower threshold regions, including bounded sequenced output" in {
      val bands = List(
        (VT.RSX(10), 50.0, 95.0, 5.0, 50.0, 3),
        (VT.WilliamsR(10), -50.0, -5.0, -95.0, -50.0, 3),
        (VT.CCI(10), 0.0, 225.0, -225.0, 0.0, 3),
        (VT.CMF(10), 0.0, 0.9, -0.9, 0.0, 3),
        (VT.sequenced(VT.CMF(10), VT.SMA(10)), 0.0, 0.9, -0.9, 0.0, 4)
      )

      bands.foreach { case (vt, upperMin, upperMax, lowerMin, lowerMax, dimensions) =>
        val a = Indicator.ThresholdCrossing(close, vt, upperMin, lowerMin)
        val d = distance(a)
        d.valueBetween(a, Indicator.ThresholdCrossing(close, vt, upperMax, lowerMin)) mustBe
          (1.0 / math.sqrt(dimensions.toDouble) +- 1e-12)
        d.valueBetween(a, Indicator.ThresholdCrossing(close, vt, upperMin, lowerMax)) mustBe
          (1.0 / math.sqrt(dimensions.toDouble) +- 1e-12)
      }
      succeed
    }

    "preserve nested schema order and weight coordinates rather than whole indicators" in {
      val a = Indicator.compositeAllOf(
        trend(VT.SMA(5)),
        Indicator.compositeAnyOf(trend(VT.sequenced(VT.JMA(5, -100, 1), VT.sequenced(VT.Kalman(0.01, 0.01)))))
      )
      val b = Indicator.compositeAllOf(
        trend(VT.SMA(100)),
        Indicator.compositeAnyOf(trend(VT.sequenced(VT.JMA(5, -100, 1), VT.sequenced(VT.Kalman(0.01, 0.01)))))
      )
      distance(a).valueBetween(a, b) mustBe (1.0 / math.sqrt(6.0) +- 1e-12)

      val ordered = trend(VT.sequenced(VT.SMA(5), VT.SMA(100)))
      distance(ordered).valueBetween(ordered, trend(VT.sequenced(VT.SMA(100), VT.SMA(5)))) mustBe (1.0 +- 1e-12)
    }

    "exclude explicit subtrees, raw close and unread trackers from the metric" in {
      val frozen = Indicator.compositeAllOf(trend(VT.JMA(10, 0, 2)))
      val raw    = Indicator.ValueTracking(ValueRole.Price, close, VT.SMA(1))
      val unread = Indicator.ValueTracking(ValueRole.Momentum, close, VT.RSX(10))
      val a      = Indicator.compositeAnyOf(frozen, raw, unread, trend(VT.SMA(5)))
      val b      = Indicator.compositeAnyOf(
        Indicator.compositeAllOf(trend(VT.JMA(100, 100, 10))),
        Indicator.ValueTracking(ValueRole.Price, close, VT.SMA(50)),
        Indicator.ValueTracking(ValueRole.Momentum, close, VT.RSX(50)),
        trend(VT.SMA(5))
      )
      val d = distance(a, Set(frozen))
      d.valueBetween(a, b) mustBe 0.0
      val c = Indicator.compositeAnyOf(frozen, raw, unread, trend(VT.SMA(100)))
      d.valueBetween(a, c) mustBe (1.0 +- 1e-12)
    }

    "return zero for entirely fixed strategies even when their numeric values differ" in {
      val a = trend(VT.JMA(10, 0, 2))
      val b = trend(VT.JMA(100, 100, 10))
      distance(a, Set(a)).valueBetween(a, b) mustBe 0.0
      val raw = Indicator.ValueTracking(ValueRole.Price, close, VT.SMA(1))
      distance(raw).valueBetween(raw, Indicator.ValueTracking(ValueRole.Price, close, VT.SMA(99))) mustBe 0.0
    }

    "reject incompatible schemas even if all leaves would otherwise be fixed" in {
      val a = trend(VT.SMA(10))
      val d = distance(a, Set(a))
      d.between(a, trend(VT.EMA(10))).left.toOption.map(_.getMessage) mustBe Some(
        "Indicator does not match this round's search-space schema"
      )
      d.between(Indicator.TrendChangeDetection(ValueSource.Open, VT.SMA(10)), a).isLeft mustBe true
      val nma = trend(VT.NMA(10, 20, 1, MovingAverage.Simple))
      distance(nma).between(nma, trend(VT.NMA(10, 20, 1, MovingAverage.Exponential))).isLeft mustBe true
      succeed
    }

    "measure out-of-bounds values without clamping or repairing relational constraints" in {
      val a = trend(VT.SMA(100))
      val b = trend(VT.SMA(200))
      distance(a).valueBetween(a, b) mustBe (math.log(2.0) / math.log(20.0) +- 1e-12)
      val crossed = Indicator.LinesCrossing(close, VT.SMA(10), VT.SMA(10))
      distance(crossed).valueBetween(crossed, Indicator.LinesCrossing(close, VT.SMA(10), VT.SMA(100))) mustBe
        (math.log(10.0) / math.log(20.0) / math.sqrt(2.0) +- 1e-12)
    }

    "reject nonfinite numeric values and nonpositive logarithmic genes" in {
      val a = trend(VT.Kalman(0.1, 0.2))
      val d = distance(a)
      List(Double.NaN, Double.PositiveInfinity, Double.NegativeInfinity).foreach { invalid =>
        d.between(a, trend(VT.Kalman(invalid, 0.2))).isLeft mustBe true
        d.between(trend(VT.Kalman(0.1, invalid)), a).isLeft mustBe true
        val threshold = Indicator.ThresholdCrossing(close, VT.CMF(10), 0.5, -0.5)
        distance(threshold).between(threshold, Indicator.ThresholdCrossing(close, VT.CMF(10), invalid, -0.5)).isLeft mustBe true
      }
      val positive = trend(VT.SMA(5))
      List(0, -1).foreach { invalid =>
        distance(positive).between(positive, trend(VT.SMA(invalid))).isLeft mustBe true
      }
      succeed
    }

    "return Left when finite genes overflow their normalisation or the resulting distance" in {
      val a = trend(VT.Kalman(0.1, 0.2))
      val d = distance(a)
      d.between(a, trend(VT.Kalman(Double.MaxValue, 0.2))).left.toOption.map(_.getMessage) mustBe
        Some("Indicator distance needs finite normalised coordinates")
      d.between(trend(VT.Kalman(0.1, -1e308)), trend(VT.Kalman(0.1, 1e308))).left.toOption.map(_.getMessage) mustBe
        Some("Indicator distance must be finite")
    }
  }
}
