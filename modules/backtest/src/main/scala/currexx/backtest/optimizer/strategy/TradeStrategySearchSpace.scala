package currexx.backtest.optimizer.strategy

import cats.syntax.traverse.*
import cats.syntax.foldable.*
import currexx.core.trade.{Rule, TradeStrategy}
import currexx.domain.signal.Indicator

import scala.util.Random

/** A fixed rule tree with numeric coordinates that can each link several matching fields. */
final class TradeStrategySearchSpace private (
    val template: TradeStrategy,
    private val parameters: Vector[LeafParameter]
) {
  import LeafParameter.Leaves

  private val parametersByPath =
    parameters.flatMap(parameter => parameter.paths.map(_.rule).distinct.map(_ -> parameter)).groupMap(_._1)(_._2)
  val parameterCount: Int = parameters.count(_.active)

  private def normaliseNumeric(path: RulePath, leaf: Rule.Condition): Either[IllegalArgumentException, Rule.Condition] =
    parametersByPath.getOrElse(path, Vector.empty).foldLeft[Either[IllegalArgumentException, Rule.Condition]](Right(leaf)) {
      (result, parameter) => result.flatMap(parameter.normaliseNumeric(path, _))
    }

  def validate(candidate: TradeStrategy): Either[IllegalArgumentException, TradeStrategy] =
    validatedLeaves(candidate).map(_ => candidate)

  def validateStructure(candidate: TradeStrategy): Either[IllegalArgumentException, Unit] =
    RuleTree.validateStructure(template, candidate)(normaliseNumeric)

  /** Fixed numeric values may be restored, but categories and active linked groups must retain their meaning. */
  def projectSeed(candidate: TradeStrategy): Either[IllegalArgumentException, TradeStrategy] =
    for
      _        <- validateStructure(candidate)
      restored <- change(RuleTree.leaves(candidate).toMap)((parameter, leaves) => parameter.restoreFixed(leaves))
      _        <- validateParameters(restored)
      result   <- rebuild(candidate, restored)
    yield result

  private[strategy] def randomise(candidate: TradeStrategy)(using Random): Either[IllegalArgumentException, TradeStrategy] =
    transform(candidate)((parameter, leaves) => parameter.sample(leaves))

  private[strategy] def nearby(candidate: TradeStrategy, radius: Double)(using Random): Either[IllegalArgumentException, TradeStrategy] =
    if (radius == 0) validate(candidate)
    else transform(candidate)((parameter, leaves) => parameter.nearby(leaves, radius))

  private[strategy] def mutate(candidate: TradeStrategy, probability: Double)(using
      Random
  ): Either[IllegalArgumentException, TradeStrategy] =
    if (probability == 0) validate(candidate)
    else transform(candidate)((parameter, leaves) => parameter.mutate(leaves, probability))

  private[strategy] def cross(first: TradeStrategy, second: TradeStrategy)(using Random): Either[IllegalArgumentException, TradeStrategy] =
    for
      firstLeaves  <- validatedLeaves(first)
      secondLeaves <- validatedLeaves(second)
      result       <- change(firstLeaves)((parameter, leaves) => parameter.cross(leaves, secondLeaves)).flatMap(rebuild(first, _))
    yield result

  private[strategy] def distance(first: TradeStrategy, second: TradeStrategy): Either[IllegalArgumentException, Double] =
    for
      firstLeaves  <- validatedLeaves(first)
      secondLeaves <- validatedLeaves(second)
      distances    <- parameters.filter(_.active).traverse(_.distance(firstLeaves, secondLeaves))
    yield if (distances.isEmpty) 0.0 else math.sqrt(distances.map(d => d * d).sum / distances.size)

  private def validatedLeaves(candidate: TradeStrategy): Either[IllegalArgumentException, Leaves] =
    for
      _ <- validateStructure(candidate)
      leaves = RuleTree.leaves(candidate).toMap
      _ <- validateParameters(leaves)
    yield leaves

  private def validateParameters(leaves: Leaves): Either[IllegalArgumentException, Unit] =
    parameters.traverse_(_.validate(leaves))

  private def rebuild(candidate: TradeStrategy, leaves: Leaves): Either[IllegalArgumentException, TradeStrategy] =
    RuleTree.mapLeaves(candidate)((path, _) => leaves.get(path).toRight(new IllegalArgumentException(s"Missing candidate leaf at $path")))

  private def change(leaves: Leaves)(
      update: (LeafParameter, Leaves) => Either[IllegalArgumentException, Leaves]
  ): Either[IllegalArgumentException, Leaves] =
    parameters.foldLeft[Either[IllegalArgumentException, Leaves]](Right(leaves))((result, parameter) =>
      result.flatMap(update(parameter, _))
    )

  private def transform(candidate: TradeStrategy)(
      update: (LeafParameter, Leaves) => Either[IllegalArgumentException, Leaves]
  ): Either[IllegalArgumentException, TradeStrategy] =
    validatedLeaves(candidate).flatMap(leaves => change(leaves)(update)).flatMap(rebuild(candidate, _))
}

object TradeStrategySearchSpace {
  def forStrategy(
      template: TradeStrategy,
      indicator: Indicator,
      bounds: RuleParameterBounds
  ): Either[IllegalArgumentException, TradeStrategySearchSpace] =
    for
      parameters <- RuleParameterCompiler.compile(template, bounds, IndicatorCapabilities.from(indicator))
      space = new TradeStrategySearchSpace(template, parameters)
      _ <- space.validate(template)
    yield space
}
