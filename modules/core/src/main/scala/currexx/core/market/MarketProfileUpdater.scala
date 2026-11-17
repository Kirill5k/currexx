package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.signal.{Boundary, Condition, Direction}

object MarketProfileUpdater:
  def update(profile: MarketProfile, signal: Signal): MarketProfile =
    signal.triggeredBy.resolvedId.fold(profile) { id =>
      def put(reading: Reading): MarketProfile = profile.updated(id, reading)
      signal.condition match
        case Condition.ValueUpdated(_, value)         => put(Reading.Value(value))
        case Condition.TrendDirectionChange(_, to, _) =>
          if profile.trend(id).exists(_.direction == to) then profile
          else put(Reading.Trend(to, signal.time))
        case Condition.LinesCrossing(direction) =>
          if profile.crossover(id).exists(_.direction == direction) then profile
          else put(Reading.Crossing(direction, signal.time))
        case Condition.ThresholdCrossing(_, _, direction, boundary) =>
          val zone = (direction, boundary) match
            case (Direction.Upward, Boundary.Upper)                                        => MomentumZone.Overbought
            case (Direction.Downward, Boundary.Lower)                                      => MomentumZone.Oversold
            case (Direction.Downward, Boundary.Upper) | (Direction.Upward, Boundary.Lower) => MomentumZone.Neutral
            case _ => profile.momentum(id).fold(MomentumZone.Neutral)(_.zone)
          if profile.momentum(id).exists(_.zone == zone) then profile
          else put(Reading.Zone(zone, signal.time))
        case Condition.VolatilityRegimeChange(_, to) =>
          if profile.volatility(id).exists(_.regime == to) then profile
          else put(Reading.Regime(to, signal.time))
        case Condition.UpperBandCrossing(direction) =>
          put(Reading.BandCrossing(Boundary.Upper, direction, signal.time))
        case Condition.LowerBandCrossing(direction) =>
          put(Reading.BandCrossing(Boundary.Lower, direction, signal.time))
        case Condition.PriceCrossedLine(_, direction) =>
          put(Reading.Crossing(direction, signal.time))
        // Historical composites remain readable, but processing accepts attributed leaves only.
        case _: Condition.Composite => profile
    }
