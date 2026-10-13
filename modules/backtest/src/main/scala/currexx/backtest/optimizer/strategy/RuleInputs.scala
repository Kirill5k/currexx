package currexx.backtest.optimizer.strategy

import cats.syntax.foldable.*
import currexx.backtest.optimizer.Checks
import currexx.core.trade.Rule
import currexx.domain.signal.ValueRole

/** One exhaustive declaration of each condition's detector and tracked-value dependencies. */
private[optimizer] object RuleInputs {
  final private case class Requirement(
      trackedRoles: Set[ValueRole],
      available: IndicatorCapabilities => Boolean,
      field: RuleParameter,
      description: String
  )

  def trackedRoles(condition: Rule.Condition): Set[ValueRole] = required(condition).flatMap(_.trackedRoles).toSet

  private[strategy] def validateLeaf(
      condition: Rule.Condition,
      path: RulePath,
      inputs: IndicatorCapabilities
  ): Either[IllegalArgumentException, Unit] =
    required(condition).traverse_ { requirement =>
      Checks.check(
        requirement.available(inputs),
        s"Missing ${requirement.description} at ${RuleParameterPath(path, requirement.field)}"
      )
    }

  private def detector(check: IndicatorCapabilities => Boolean, field: RuleParameter, description: String): List[Requirement] =
    List(Requirement(Set.empty, check, field, description))

  private def value(role: ValueRole, field: RuleParameter, description: String): List[Requirement] =
    List(Requirement(Set(role), _.valueRoles.contains(role), field, description))

  private def required(condition: Rule.Condition): List[Requirement] = {
    import Rule.Condition as C
    condition match
      case C.AllOf(conditions)                           => conditions.flatMap(required)
      case C.AnyOf(conditions)                           => conditions.flatMap(required)
      case C.Not(inner)                                  => required(inner)
      case _: C.TrendChangedTo | _: C.TrendIs            => detector(_.trend, RuleParameter.Direction, "trend detector")
      case _: C.TrendActiveFor                           => detector(_.trend, RuleParameter.Duration, "trend detector")
      case _: C.CrossoverOccurred                        => detector(_.crossover, RuleParameter.Direction, "line crossover detector")
      case _: C.MomentumEntered | _: C.MomentumIsIn      => detector(_.momentumZone, RuleParameter.Zone, "momentum threshold detector")
      case _: C.MomentumIs                               => value(ValueRole.Momentum, RuleParameter.Direction, "momentum values")
      case _: C.PreviousVolatilityIs | _: C.VolatilityIs => detector(_.volatility, RuleParameter.Regime, "volatility regime detector")
      case _: C.VelocityIs                               => value(ValueRole.Velocity, RuleParameter.Direction, "velocity values")
      case _: C.VelocityCrossedLevel                     => value(ValueRole.Velocity, RuleParameter.Level, "velocity values")
      case _: C.VelocityIsBelow                          => value(ValueRole.Velocity, RuleParameter.Threshold, "velocity values")
      case _: C.PositionIs | _: C.PositionOpenFor | C.NoPosition => Nil
      case _: C.UpperBandCrossed | _: C.LowerBandCrossed         => detector(_.bands, RuleParameter.Direction, "band detector")
      case C.PriceCrossedLine(role, _)                           =>
        detector(_.lineRoles.contains(role), RuleParameter.Role, s"price-line crossing detector for $role")
      case C.ValueIs(role, _, _)       => value(role, RuleParameter.Role, s"tracked values for fixed ValueIs role $role")
      case _: C.PriceMovedAgainstEntry =>
        List(
          Requirement(
            Set(ValueRole.Price, ValueRole.Volatility),
            _.priceStop,
            RuleParameter.AtrMultiplier,
            "raw close and ATR value trackers without conflicting writers"
          )
        )
  }
}
