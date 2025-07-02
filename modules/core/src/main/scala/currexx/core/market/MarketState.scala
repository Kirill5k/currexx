package currexx.core.market

import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.{Condition, Direction}
import currexx.domain.user.UserId
import io.circe.Codec

import java.time.Instant


//TODO: REFINE: This is a simplified version of the market state.
final case class MarketProfile(
    // Trend-related view
    trendDirection: Option[Direction] = None, // e.g., from TrendChangeDetection
    trendStrength: Option[Double] = None,     // Could be length, or Kalman velocity

    // Crossover-related view
    crossoverSignal: Option[Direction] = None, // e.g., from LinesCrossing

    // Oscillator-related view
    isInOverboughtZone: Option[Boolean] = None, // e.g., from ThresholdCrossing > upper
    isInOversoldZone: Option[Boolean] = None,   // e.g., from ThresholdCrossing < lower

    // Volatility-related view
    volatilityCondition: Option[Condition] = None // e.g., Keltner Channel band cross
) derives Codec.AsObject

final case class PositionState(
    position: TradeOrder.Position,
    openedAt: Instant,
    openPrice: BigDecimal
) derives Codec.AsObject

final case class MarketState(
    userId: UserId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    profile: MarketProfile,
    lastUpdatedAt: Instant,
    createdAt: Instant
) derives Codec.AsObject {
  def hasOpenPosition: Boolean = currentPosition.isDefined
}
