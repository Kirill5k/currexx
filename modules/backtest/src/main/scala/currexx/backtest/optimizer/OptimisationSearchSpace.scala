package currexx.backtest.optimizer

import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.indicator.{IndicatorDistance, IndicatorSearchSpace}
import currexx.backtest.optimizer.strategy.TradeStrategySearchSpace
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator

/** A complete candidate has one identity regardless of which component this round searches. */
final class OptimisationSearchSpace private (
    val template: TestStrategy,
    val components: OptimisationSearchSpace.Components
) {
  import OptimisationSearchSpace.{Components, ResolvedSeed, SeedDisposition}

  val target: OptimisationTarget = components match
    case Components.IndicatorOnly(_) => OptimisationTarget.Indicator
    case Components.RulesOnly(_)     => OptimisationTarget.Strategy
    case Components.Joint(_, _)      => OptimisationTarget.Both
  def indicatorSpace: Option[IndicatorSearchSpace] = components match
    case Components.IndicatorOnly(indicators) => Some(indicators)
    case Components.Joint(indicators, _)      => Some(indicators)
    case Components.RulesOnly(_)              => None
  def strategySpace: Option[TradeStrategySearchSpace] = components match
    case Components.RulesOnly(rules) => Some(rules)
    case Components.Joint(_, rules)  => Some(rules)
    case Components.IndicatorOnly(_) => None

  val indicatorParameterCount: Int    = indicatorSpace.fold(0)(IndicatorDistance.parameterCount)
  val strategyParameterCount: Int     = strategySpace.fold(0)(_.parameterCount)
  val parameterCount: Int             = indicatorParameterCount + strategyParameterCount
  val fixedIndicators: Set[Indicator] = indicatorSpace.fold(Set(template.indicator))(_.fixedIndicators)

  def canonicalise(candidate: TestStrategy): Either[IllegalArgumentException, TestStrategy] =
    restoreComponents(candidate)(rules =>
      strategySpace.fold[Either[IllegalArgumentException, TradeStrategy]](Right(template.rules))(_.validate(rules))
    )

  private def restoreComponents(candidate: TestStrategy)(
      ruleValues: TradeStrategy => Either[IllegalArgumentException, TradeStrategy]
  ): Either[IllegalArgumentException, TestStrategy] =
    for
      indicator <- indicatorSpace.fold[Either[IllegalArgumentException, Indicator]](Right(template.indicator))(
        _.canonicalise(candidate.indicator)
      )
      rules <- ruleValues(candidate.rules)
    yield TestStrategy(indicator, rules)

  /** Configured seeds may carry different fixed values; restore them before checking the active rule domains. */
  private def projectSeed(candidate: TestStrategy): Either[IllegalArgumentException, (TestStrategy, Option[String])] = {
    val replacedRules = strategySpace
      .filter(_ => target == OptimisationTarget.Both)
      .flatMap(_.validateStructure(candidate.rules).swap.toOption)
    val reason = replacedRules.map(error => s"Using seed indicators with base rules: ${error.getMessage}")
    restoreComponents(candidate) { rules =>
      if (replacedRules.nonEmpty) Right(template.rules)
      else strategySpace.fold[Either[IllegalArgumentException, TradeStrategy]](Right(template.rules))(_.projectSeed(rules))
    }.map(_ -> reason)
  }

  def resolveSeeds(extraSeeds: List[TestStrategy]): Either[IllegalArgumentException, List[ResolvedSeed]] =
    canonicalise(template).flatMap { base =>
      extraSeeds.zipWithIndex
        .foldLeft[Either[IllegalArgumentException, (List[ResolvedSeed], Map[TestStrategy, Int])]](Right((Nil, Map.empty))) {
          case (acc, (seed, index)) =>
            acc.flatMap { case (resolved, seen) =>
              projectSeed(seed) match
                case Left(error) =>
                  Right((ResolvedSeed(index, None, SeedDisposition.Incompatible, Some(error.getMessage)) :: resolved, seen))
                case Right((effective, ruleSubstitution)) =>
                  val disposition =
                    if (effective == base) SeedDisposition.TargetDuplicate
                    else seen.get(effective).fold[SeedDisposition](SeedDisposition.Accepted)(SeedDisposition.SeedDuplicate.apply)
                  val firstIndex = seen.updatedWith(effective)(_.orElse(Some(index)))
                  val projected  =
                    ResolvedSeed(index, Some(effective), disposition, ruleSubstitution, usedBaseRules = ruleSubstitution.nonEmpty)
                  Right((projected :: resolved, firstIndex))
            }
        }
        .map(_._1.reverse)
    }

  def projectSeeds(extraSeeds: List[TestStrategy]): Either[IllegalArgumentException, List[TestStrategy]] =
    resolveSeeds(extraSeeds).map(_.filter(_.disposition == SeedDisposition.Accepted).flatMap(_.effective))
}

object OptimisationSearchSpace {
  enum SeedDisposition {
    case Accepted, TargetDuplicate, Incompatible
    case SeedDuplicate(index: Int)
  }

  final case class ResolvedSeed(
      index: Int,
      effective: Option[TestStrategy],
      disposition: SeedDisposition,
      reason: Option[String] = None,
      usedBaseRules: Boolean = false
  )

  enum Components {
    case IndicatorOnly(indicators: IndicatorSearchSpace)
    case RulesOnly(rules: TradeStrategySearchSpace)
    case Joint(indicators: IndicatorSearchSpace, rules: TradeStrategySearchSpace)
  }

  def forRound(round: OptimisationRound): Either[IllegalArgumentException, OptimisationSearchSpace] = {
    def ruleSpace: Either[IllegalArgumentException, TradeStrategySearchSpace] =
      TradeStrategySearchSpace
        .forStrategy(round.strategy.rules, round.strategy.indicator, round.ruleBounds)
    val components = round.target match
      case OptimisationTarget.Indicator =>
        IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators).map(Components.IndicatorOnly.apply)
      case OptimisationTarget.Strategy =>
        for
          _ <- Checks.check(
            round.fixedIndicators.isEmpty,
            "fixedIndicators is not supported for Strategy rounds; all indicators are already fixed"
          )
          rules <- ruleSpace
          _     <- Checks.check(
            rules.parameterCount > 0,
            "Strategy rounds require at least one active rule parameter; configure a non-singleton rule bound"
          )
        yield Components.RulesOnly(rules)
      case OptimisationTarget.Both =>
        for
          rules      <- ruleSpace
          indicators <- IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators)
        yield Components.Joint(indicators, rules)
    components.map(new OptimisationSearchSpace(round.strategy, _))
  }
}
