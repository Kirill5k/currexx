package currexx.core.market

import currexx.core.fixtures.{Markets, Users}
import currexx.core.signal.{Signal, SignalBatch}
import currexx.core.trade.{Rule, TradeAction}
import currexx.domain.market.{Interval, TradeOrder}
import currexx.domain.signal.{Boundary, Condition, Direction, Indicator, IndicatorId, ValueRole, ValueSource, ValueTransformation as VT}
import io.circe.parser.decode
import io.circe.syntax.*
import org.scalatest.OptionValues
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class ProfileTransitionSpec extends AnyWordSpec with Matchers with OptionValues:
  private val time                         = Instant.parse("2026-01-01T12:00:00Z")
  private val cmf: Indicator.ValueTracking = Indicator.ValueTracking(ValueRole.Momentum, ValueSource.Close, VT.CMF(20))
  private val trend                        = Indicator.TrendChangeDetection(ValueSource.Close, VT.EMA(20))
  private val zone                         = Indicator.ThresholdCrossing(ValueSource.Close, VT.RSX(14), 70, 30)
  private val sources                      = List(cmf, trend, zone).map(i => i.resolvedId.value -> i).toMap
  private val momentum                     = IndicatorId.forValue(ValueRole.Momentum)
  private val state                        = MarketState
    .initial(Users.uid, Markets.gbpeur, time)
    .copy(
      configRevision = Some(2L),
      sources = sources,
      interval = Some(Interval.H1),
      lastCandleTime = Some(time),
      lastTimeStateCandle = Some(time),
      profile = MarketProfile()
        .updated(IndicatorId.Trend, Reading.Trend(Direction.Upward, time))
        .updated(momentum, Reading.Value(BigDecimal("0.1"))),
      previousProfile = Some(MarketProfile().updated(momentum, Reading.Value(BigDecimal("0.05"))))
    )
  private def signal(indicator: Indicator, condition: Condition, at: Instant = time.plusSeconds(3600)): Signal =
    Signal(Users.uid, Markets.gbpeur, Interval.H1, condition, indicator, at)
  private def batch(signals: List[Signal] = Nil): SignalBatch =
    SignalBatch(
      interval = Interval.H1,
      revision = 2L,
      candleTime = time.plusSeconds(3600),
      closureGap = None,
      sources = sources,
      signals = signals
    )
  private def fires(condition: Rule.Condition, current: MarketState): Boolean =
    Rule
      .findTriggeredAction(List(Rule(TradeAction.ClosePosition, condition)), current, current.previousProfile.getOrElse(MarketProfile()))
      .nonEmpty

  "A source-keyed profile" should {
    "keep RSX zones and CMF numbers independent in every signal order" in {
      val updates = List(
        signal(zone, Condition.ThresholdCrossing(70, 80, Direction.Upward, Boundary.Upper)),
        signal(cmf, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.25")))
      )
      val profiles = updates.permutations.map(_.foldLeft(MarketProfile())(MarketProfileUpdater.update)).toList
      profiles.distinct.size mustBe 1
      profiles.head.number(momentum) mustBe Some(BigDecimal("0.25"))
      profiles.head.momentum().map(_.zone) mustBe Some(MomentumZone.Overbought)
    }

    "keep two explicitly named trackers independent and readable by rules" in {
      val fast = Indicator.Named(IndicatorId("fast"), cmf)
      val slow = Indicator.Named(IndicatorId("slow"), cmf.copy(transformation = VT.CMF(40)))
      val p    = List(
        signal(fast, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.1"))),
        signal(slow, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("-0.2")))
      ).foldLeft(MarketProfile())(MarketProfileUpdater.update)
      p.readings.size mustBe 2
      fires(Rule.Condition.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 0, fast.resolvedId), state.copy(profile = p)) mustBe true
      fires(Rule.Condition.ValueIs(ValueRole.Momentum, Rule.Operator.GreaterThan, 0, slow.resolvedId), state.copy(profile = p)) mustBe false
    }

    "retain the legacy JSON for unnamed indicator and rule definitions" in {
      (cmf: Indicator).asJson.hcursor.downField("id").succeeded mustBe false
      (Rule.Condition.MomentumIs(Direction.Upward): Rule.Condition).asJson.hcursor.downField("of").succeeded mustBe false
      decode[Indicator]((cmf: Indicator).asJson.noSpaces) mustBe Right(cmf)
      val named = Indicator.Named(IndicatorId("flow"), cmf)
      decode[Indicator](named.asJson.noSpaces) mustBe Right(named)
    }
  }

  "The profile transition" should {
    "reconcile an empty newer batch even for an already applied candle" in {
      val changed = cmf.copy(transformation = VT.CMF(25))
      val result  =
        ProfileTransition(state, batch().copy(revision = 3L, candleTime = time, sources = sources.updated(momentum, changed)))
      result.state.profile.trend() mustBe state.profile.trend()
      result.state.profile.number(momentum) mustBe None
      result.state.previousProfile.value.number(momentum) mustBe None
      result.state.invalidated must contain(momentum)
      result.state.configRevision mustBe Some(3L)
      result.state.lastCandleTime mustBe Some(time)
      result.outcome mustBe ProfileTransition.Outcome.Skipped
    }

    "forget invalidations for removed sources, including stale ids in an unchanged configuration" in {
      val removed = batch().copy(sources = sources - momentum)
      val result  = ProfileTransition(state.copy(invalidated = Set(momentum)), removed)
      result.state.profile.number(momentum) mustBe None
      result.state.previousProfile.value.number(momentum) mustBe None
      result.state.invalidated mustBe empty
      result.outcome mustBe ProfileTransition.Outcome.Unchanged

      val retired   = state.copy(invalidated = Set(IndicatorId("retired")))
      val unchanged = ProfileTransition(retired, batch()).state
      unchanged.invalidated mustBe empty
      (unchanged.profile eq state.profile) mustBe true
    }

    "apply closure shifts without replacing history or evaluating rules on an empty batch" in {
      val result = ProfileTransition(state, batch().copy(closureGap = Some(71.hours)))
      result.state.profile.trend().value.confirmedAt mustBe time.plusSeconds(71 * 3600)
      result.state.previousProfile mustBe state.previousProfile
      result.outcome mustBe ProfileTransition.Outcome.Unchanged
    }

    "not repeat a closure adjustment persisted by the old pipeline before its signals" in {
      val at      = time.plusSeconds(3600)
      val shifted = state.copy(lastTimeStateCandle = Some(at))
      val update  = signal(cmf, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.3")))
      val result  = ProfileTransition(shifted, batch(List(update)).copy(closureGap = Some(71.hours)))
      result.state.profile.trend() mustBe shifted.profile.trend()
      result.state.profile.number(momentum) mustBe Some(BigDecimal("0.3"))
      result.state.lastCandleTime mustBe Some(at)
      result.outcome mustBe ProfileTransition.Outcome.Changed
    }

    "keep position state across an interval reset and invalidate discarded readings" in {
      val position = PositionState(TradeOrder.Position.Buy, time, Some(BigDecimal("1.2")))
      val result   = ProfileTransition(
        state.copy(currentPosition = Some(position)),
        batch().copy(interval = Interval.M5)
      ).state
      result.currentPosition mustBe Some(position)
      result.profile.readings mustBe empty
      result.previousProfile mustBe None
      result.invalidated must contain(momentum)
    }

    "discard a legacy momentum slot when a threshold could have written it" in {
      val old = MarketState
        .initial(Users.uid, Markets.gbpeur, time)
        .copy(profile = MarketProfile.fromLegacy(LegacyMarketProfile(lastMomentumValue = Some(BigDecimal(85)))))
      val result = ProfileTransition(old, batch()).state
      result.profile.number(momentum) mustBe None
      result.invalidated must contain(momentum)
    }

    "accept a signal after an empty poll of the same candle without repeating its closure gap" in {
      val empty = ProfileTransition(state, batch().copy(closureGap = Some(71.hours))).state
      empty.lastCandleTime mustBe Some(time)
      val update  = signal(cmf, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.3")))
      val emitted = ProfileTransition(empty, batch(List(update)).copy(closureGap = Some(71.hours)))
      emitted.outcome mustBe ProfileTransition.Outcome.Changed
      emitted.state.profile.trend() mustBe empty.profile.trend()
      emitted.state.profile.number(momentum) mustBe Some(BigDecimal("0.3"))
      val replay = ProfileTransition(emitted.state, batch(List(update)))
      replay.state mustBe emitted.state
      replay.outcome mustBe ProfileTransition.Outcome.Skipped
    }

    "reject emissions older than the latest empty poll" in {
      val empty  = ProfileTransition(state, batch().copy(candleTime = time.plusSeconds(7200))).state
      val update = signal(cmf, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.3")))
      val result = ProfileTransition(empty, batch(List(update)))
      result.state mustBe empty
      result.outcome mustBe ProfileTransition.Outcome.Skipped
    }

    "reuse both profiles when their source definitions have not changed" in {
      val result = ProfileTransition(state, batch()).state
      (result.profile eq state.profile) mustBe true
      (result.previousProfile.value eq state.previousProfile.value) mustBe true
    }

    "retain invalidation when equal samples do not establish a previous profile" in {
      val empty  = state.copy(profile = MarketProfile(), previousProfile = None, invalidated = Set(momentum))
      val update = signal(cmf, Condition.ValueUpdated(ValueRole.Momentum, BigDecimal("0.1")))
      val first  = ProfileTransition(empty, batch(List(update))).state
      val second = ProfileTransition(
        first,
        batch(List(update.copy(time = time.plusSeconds(7200)))).copy(candleTime = time.plusSeconds(7200))
      ).state
      second.previousProfile.flatMap(_.number(momentum)) mustBe None
      second.invalidated must contain(momentum)
    }
  }
