package currexx.core.trade

import currexx.domain.signal.{Direction, Indicator, IndicatorId, ValueRole, ValueSource, ValueTransformation as VT}
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StrategyCheckSpec extends AnyWordSpec with Matchers with OptionValues:
  private val cmf      = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.CMF(20))
  private val trend    = Indicator.TrendChangeDetection(ValueSource.Close, VT.EMA(20))
  private val zone     = Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(14), 70, 30)
  private val sources  = List(cmf, trend, zone).map(i => i.resolvedId.value -> i).toMap
  private val momentum = IndicatorId.forValue(ValueRole.Momentum)

  "StrategyCheck" should {
    "require a single unnamed leaf inside Named" in {
      StrategyCheck
        .validate(List(Indicator.Named(IndicatorId("nested"), Indicator.Named(IndicatorId("inner"), cmf))), None)
        .isLeft mustBe true
      StrategyCheck.validate(List(Indicator.Named(IndicatorId("group"), Indicator.compositeAnyOf(cmf, trend))), None).isLeft mustBe true
    }

    "reject references with an incompatible kind, role or ATR transformation" in {
      StrategyCheck.validateCondition(Rule.Condition.TrendIs(Direction.Upward, Some(momentum)), sources).isLeft mustBe true
      StrategyCheck.validateCondition(Rule.Condition.VelocityIs(Direction.Upward, Some(momentum)), sources).isLeft mustBe true
      val price     = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
      val deviation = Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.StandardDeviation(14))
      val wrongStop = List(price, deviation).map(i => i.resolvedId.value -> i).toMap
      StrategyCheck.validateCondition(Rule.Condition.PriceMovedAgainstEntry(2), wrongStop).isLeft mustBe true
    }

    "accept only lowercase letters, digits and hyphens in indicator ids" in {
      List("", " ", "$date", "flow.value", "Flow", "flow_value", "flow/value", "flow\u0000value").foreach { id =>
        withClue(s"id=$id: ") {
          StrategyCheck.validate(List(Indicator.Named(IndicatorId(id), cmf)), None).isLeft mustBe true
        }
      }
      List("cmf", "cmf-20", "123").foreach { id =>
        StrategyCheck.validate(List(Indicator.Named(IndicatorId(id), cmf)), None).isRight mustBe true
      }
    }

    "reject duplicate ids, reserved ids and incompatible stop sources" in {
      StrategyCheck.validate(List(cmf, cmf), None).isLeft mustBe true
      StrategyCheck.validate(List(Indicator.Named(IndicatorId("$date"), cmf)), None).isLeft mustBe true
      StrategyCheck.validate(List(Indicator.Named(IndicatorId.Trend, cmf)), None).isLeft mustBe true
      val stop = TradeStrategy(Nil, List(Rule(TradeAction.ClosePosition, Rule.Condition.PriceMovedAgainstEntry(2))))
      val smoothed: Indicator.ValueTracking = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(20))
      val atr                               = Indicator.ValueTracking(ValueRole.Volatility, ValueSource.Close, VT.ATR(14))
      StrategyCheck.validate(List(smoothed, atr), Some(stop)).isLeft mustBe true
      StrategyCheck.validate(List(smoothed.copy(transformation = VT.SMA(1)), atr), Some(stop)).isRight mustBe true
    }
  }
