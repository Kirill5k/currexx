package currexx.core.market

import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.market.TradeOrder
import currexx.domain.signal.{Boundary, Direction, ValueRole}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MarketStateSpec extends AnyWordSpec with Matchers {

  val ts: Instant         = Instant.parse("2026-01-02T20:00:00Z")
  val reopenedAt: Instant = ts.plusSeconds(72 * 3600)
  val gap: FiniteDuration = 71.hours

  val state: MarketState = MarketState.initial(Users.uid, Markets.gbpeur, ts).copy(
    profile = MarketProfile(trend = Some(TrendState(Direction.Upward, ts))),
    lastCandleTime = Some(ts)
  )

  "MarketState.applyCandleSignals" should {
    "apply signals from a newer candle and record it" in {
      val candleTime = ts.plusSeconds(3600)
      val signals    = List(Signals.trend(Direction.Downward, time = candleTime), Signals.crossover(Direction.Upward, time = candleTime))

      val updated = state.applyCandleSignals(signals, candleTime)

      updated mustBe Some(
        state.copy(
          profile = state.profile.copy(
            trend = Some(TrendState(Direction.Downward, candleTime)),
            crossover = Some(CrossoverState(Direction.Upward, candleTime))
          ),
          previousProfile = Some(state.profile),
          lastCandleTime = Some(candleTime)
        )
      )
    }

    "reject older and repeated candles" in {
      state.applyCandleSignals(List(Signals.trend(Direction.Downward, time = ts.minusSeconds(3600))), ts.minusSeconds(3600)) mustBe None
      state.applyCandleSignals(List(Signals.trend(Direction.Downward, time = ts)), ts) mustBe None
    }

    "record the candle without replacing previousProfile when the profile is unchanged" in {
      val candleTime = ts.plusSeconds(3600)
      val withPrev   = state.copy(previousProfile = Some(MarketProfile()))

      withPrev.applyCandleSignals(List(Signals.trend(Direction.Upward, time = candleTime)), candleTime) mustBe
        Some(withPrev.copy(lastCandleTime = Some(candleTime)))
    }

    "ignore the time shift watermark" in {
      val candleTime = ts.plusSeconds(3600)
      val shifted    = state.copy(lastTimeStateCandle = Some(candleTime))

      shifted.applyCandleSignals(List(Signals.trend(Direction.Downward, time = candleTime)), candleTime).map(_.lastCandleTime) mustBe
        Some(Some(candleTime))
    }
  }

  "MarketState.applyManualSignal" should {
    "apply a signal regardless of its time without moving the candle watermark" in {
      val manual = Signals.trend(Direction.Downward, time = ts.minusSeconds(3600))

      state.applyManualSignal(manual) mustBe Some(
        state.copy(
          profile = MarketProfile(trend = Some(TrendState(Direction.Downward, manual.time))),
          previousProfile = Some(state.profile)
        )
      )
    }

    "return None when the signal leaves the profile unchanged" in {
      state.applyManualSignal(Signals.trend(Direction.Upward)) mustBe None
    }
  }

  "MarketState.applyTimeState" should {
    val profile = MarketProfile(
      trend = Some(TrendState(Direction.Upward, ts)),
      momentum = Some(MomentumState(MomentumZone.Overbought, ts)),
      crossover = Some(CrossoverState(Direction.Upward, ts)),
      lastBandCrossing = Some(BandCrossingState(Boundary.Upper, Direction.Upward, ts)),
      lastPriceLineCrossing = Some(PriceLineCrossingState(ValueRole.Price, Direction.Upward, ts)),
      lastClosePrice = Some(BigDecimal("1.25"))
    )
    val withPosition = state.copy(profile = profile, currentPosition = Some(PositionState(TradeOrder.Position.Buy, ts)))

    "shift timestamps, clear crossings and record the candle after a market closure" in {
      val shiftedAt = ts.plusSeconds(71 * 3600)

      withPosition.applyTimeState(reopenedAt, Some(gap)) mustBe Some(
        withPosition.copy(
          profile = profile.copy(
            trend = Some(TrendState(Direction.Upward, shiftedAt)),
            momentum = Some(MomentumState(MomentumZone.Overbought, shiftedAt)),
            crossover = None,
            lastBandCrossing = None,
            lastPriceLineCrossing = None
          ),
          currentPosition = Some(PositionState(TradeOrder.Position.Buy, shiftedAt)),
          lastTimeStateCandle = Some(reopenedAt)
        )
      )
    }

    "leave state confirmed after the market reopened untouched" in {
      val later = reopenedAt.plusSeconds(60)
      val fresh = withPosition.copy(
        profile = profile.copy(
          trend = Some(TrendState(Direction.Downward, later)),
          crossover = Some(CrossoverState(Direction.Downward, later))
        ),
        currentPosition = Some(PositionState(TradeOrder.Position.Sell, later))
      )

      val updated = fresh.applyTimeState(reopenedAt, Some(gap))

      updated.flatMap(_.profile.trend) mustBe Some(TrendState(Direction.Downward, later))
      updated.flatMap(_.profile.crossover) mustBe Some(CrossoverState(Direction.Downward, later))
      updated.flatMap(_.currentPosition) mustBe Some(PositionState(TradeOrder.Position.Sell, later))
    }

    "only record the candle when there is no market closure" in {
      withPosition.applyTimeState(reopenedAt, None) mustBe Some(withPosition.copy(lastTimeStateCandle = Some(reopenedAt)))
    }

    "reject a candle that was already applied or is older" in {
      val applied = withPosition.copy(lastTimeStateCandle = Some(reopenedAt))

      applied.applyTimeState(reopenedAt, Some(gap)) mustBe None
      applied.applyTimeState(reopenedAt.minusSeconds(3600), None) mustBe None
    }

    "ignore the signals watermark" in {
      withPosition.copy(lastCandleTime = Some(reopenedAt)).applyTimeState(reopenedAt, None).map(_.lastTimeStateCandle) mustBe
        Some(Some(reopenedAt))
    }
  }
}
