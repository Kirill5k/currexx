package currexx.backtest.optimizer.strategy

import currexx.core.trade.TradeStrategy

import scala.concurrent.duration.*

enum RuleList:
  case Open, Close

/** Child indices follow the stored condition order. Not has one child, at index zero. */
final case class RulePath(list: RuleList, ruleIndex: Int, conditions: Vector[Int] = Vector.empty):
  def child(index: Int): RulePath = copy(conditions = conditions :+ index)

enum RuleParameter:
  case Direction, Zone, Regime, Position, Role, Operator, Duration, Level, Threshold, AtrMultiplier

final case class RuleParameterPath(rule: RulePath, parameter: RuleParameter)

/** Named targets link occurrences with the same base value. At selects a field in an exact strategy snapshot. */
enum RuleTarget:
  case AtrStop, TrendAge, HoldingTime
  case At(path: RuleParameterPath, template: TradeStrategy)

/** Omitted parameters stay fixed. Targets must select disjoint fields. */
final case class RuleParameterBounds(
    numeric: Map[RuleTarget, RuleParameterBounds.DoubleRange] = Map.empty,
    durations: Map[RuleTarget, RuleParameterBounds.DurationRange] = Map.empty
):
  def isEmpty: Boolean = numeric.isEmpty && durations.isEmpty

object RuleParameterBounds:
  final case class DoubleRange(min: Double, max: Double, step: Double)
  final case class DurationRange(min: FiniteDuration, max: FiniteDuration, step: FiniteDuration)

  /** Research defaults for common rule parameters. Value levels and velocity thresholds need indicator-specific ranges. */
  val default: RuleParameterBounds = RuleParameterBounds(
    numeric = Map(
      RuleTarget.AtrStop -> DoubleRange(0.5, 20.0, 0.5)
    ),
    durations = Map(
      RuleTarget.TrendAge    -> DurationRange(0.hours, 2.days, 1.hour),
      RuleTarget.HoldingTime -> DurationRange(0.hours, 2.days, 1.hour)
    )
  )
