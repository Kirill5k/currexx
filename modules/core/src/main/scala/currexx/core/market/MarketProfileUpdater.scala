package currexx.core.market

import currexx.core.signal.Signal
import currexx.domain.signal.{Boundary, Condition, Direction, ValueRole}

object MarketProfileUpdater:
  def update(profile: MarketProfile, signal: Signal): MarketProfile =
    signal.condition match {
      case Condition.ValueUpdated(role, value) =>
        role match
          case ValueRole.Momentum          => profile.copy(lastMomentumValue = Some(value))
          case ValueRole.Volatility        => profile.copy(lastVolatilityValue = Some(value))
          case ValueRole.Velocity          => profile.copy(lastVelocityValue = Some(value))
          case ValueRole.ChannelMiddleBand => profile.copy(lastChannelMiddleBandValue = Some(value))

      case Condition.TrendDirectionChange(_, to, _) =>
        val currentTrendDirection = profile.trend.map(_.direction)
        if (currentTrendDirection.isEmpty || !currentTrendDirection.contains(to)) {
          profile.copy(trend = Some(TrendState(direction = to, confirmedAt = signal.time)))
        } else profile

      case Condition.LinesCrossing(direction) =>
        val currentCrossoverDirection = profile.crossover.map(_.direction)
        if (currentCrossoverDirection.isEmpty || !currentCrossoverDirection.contains(direction)) {
          profile.copy(crossover = Some(CrossoverState(direction = direction, confirmedAt = signal.time)))
        } else profile

      case Condition.ThresholdCrossing(_, value, direction, boundary) =>
        val newZone = (direction, boundary) match {
          case (Direction.Upward, Boundary.Upper)   => MomentumZone.Overbought
          case (Direction.Downward, Boundary.Lower) => MomentumZone.Oversold
          case (Direction.Downward, Boundary.Upper) => MomentumZone.Neutral
          case (Direction.Upward, Boundary.Lower)   => MomentumZone.Neutral
          case _                                    => profile.momentum.map(_.zone).getOrElse(MomentumZone.Neutral)
        }
        val currentZone = profile.momentum.map(_.zone)
        if (!currentZone.contains(newZone)) {
          profile.copy(
            momentum = Some(MomentumState(zone = newZone, confirmedAt = signal.time)),
            lastMomentumValue = Some(value)
          )
        } else {
          profile.copy(lastMomentumValue = Some(value))
        }

      case Condition.VolatilityRegimeChange(_, to) =>
        val currentRegime = profile.volatility.map(_.regime)
        if (!currentRegime.contains(to)) {
          profile.copy(volatility = Some(VolatilityState(regime = to, confirmedAt = signal.time)))
        } else profile

      case Condition.UpperBandCrossing(direction) =>
        profile.copy(lastBandCrossing = Some(BandCrossingState(Boundary.Upper, direction, signal.time)))

      case Condition.LowerBandCrossing(direction) =>
        profile.copy(lastBandCrossing = Some(BandCrossingState(Boundary.Lower, direction, signal.time)))

      case Condition.PriceCrossedLine(role, direction) =>
        profile.copy(lastPriceLineCrossing = Some(PriceLineCrossingState(role, direction, signal.time)))

      case Condition.Composite(conditions) =>
        conditions.foldLeft(profile) { (current, innerCondition) =>
          update(current, signal.copy(condition = innerCondition))
        }
    }
