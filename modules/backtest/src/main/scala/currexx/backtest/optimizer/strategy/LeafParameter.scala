package currexx.backtest.optimizer.strategy

import cats.syntax.traverse.*
import currexx.backtest.optimizer.Checks
import currexx.core.trade.Rule

import scala.util.Random

/** One search coordinate, written to one or more linked numeric fields. */
private[strategy] trait LeafParameter {
  def paths: Vector[RuleParameterPath]
  def order: Int
  def active: Boolean
  def normaliseNumeric(path: RulePath, leaf: Rule.Condition): Either[IllegalArgumentException, Rule.Condition]
  def validate(leaves: LeafParameter.Leaves): Either[IllegalArgumentException, Unit]
  def restoreFixed(leaves: LeafParameter.Leaves): Either[IllegalArgumentException, LeafParameter.Leaves]
  def sample(leaves: LeafParameter.Leaves)(using Random): Either[IllegalArgumentException, LeafParameter.Leaves]
  def mutate(leaves: LeafParameter.Leaves, probability: Double)(using Random): Either[IllegalArgumentException, LeafParameter.Leaves]
  def nearby(leaves: LeafParameter.Leaves, radius: Double)(using Random): Either[IllegalArgumentException, LeafParameter.Leaves]
  def cross(first: LeafParameter.Leaves, second: LeafParameter.Leaves)(using Random): Either[IllegalArgumentException, LeafParameter.Leaves]
  def distance(first: LeafParameter.Leaves, second: LeafParameter.Leaves): Either[IllegalArgumentException, Double]

}

private[strategy] object LeafParameter {
  type Leaves = Map[RulePath, Rule.Condition]

  final case class Field[A](
      path: RuleParameterPath,
      target: Option[RuleTarget],
      order: Int,
      base: A,
      read: PartialFunction[Rule.Condition, A],
      update: PartialFunction[(Rule.Condition, A), Rule.Condition],
      valid: A => Boolean = (_: A) => true
  ) {
    def matches(requested: RuleTarget): Boolean = requested match
      case RuleTarget.At(selected, _) => path == selected
      case named                      => target.contains(named)
    def get(leaves: Leaves): Either[IllegalArgumentException, A] =
      leaves.get(path.rule).flatMap(read.lift).toRight(new IllegalArgumentException(s"Wrong condition at $path"))
    def set(leaves: Leaves, value: A): Either[IllegalArgumentException, Leaves] =
      leaves
        .get(path.rule)
        .flatMap(leaf => update.lift((leaf, value)))
        .toRight(new IllegalArgumentException(s"Wrong condition at $path"))
        .map(leaf => leaves.updated(path.rule, leaf))
    def restoreBase(leaf: Rule.Condition): Either[IllegalArgumentException, Rule.Condition] =
      update.lift((leaf, base)).toRight(new IllegalArgumentException(s"Wrong condition at $path"))
  }

  def apply[A](fields: Vector[Field[A]], domain: ParameterDomain[A]): LeafParameter = new LeafParameter {
    override val paths: Vector[RuleParameterPath]                                       = fields.map(_.path)
    override val order: Int                                                             = fields.head.order
    override val active: Boolean                                                        = domain.active
    private def get(leaves: Leaves): Either[IllegalArgumentException, A]                = fields.head.get(leaves)
    private def set(leaves: Leaves, value: A): Either[IllegalArgumentException, Leaves] =
      fields.foldLeft[Either[IllegalArgumentException, Leaves]](Right(leaves))((result, field) => result.flatMap(field.set(_, value)))
    override def normaliseNumeric(path: RulePath, leaf: Rule.Condition): Either[IllegalArgumentException, Rule.Condition] =
      fields.filter(_.path.rule == path).foldLeft[Either[IllegalArgumentException, Rule.Condition]](Right(leaf)) { (result, field) =>
        result.flatMap(field.restoreBase)
      }
    override def validate(leaves: Leaves): Either[IllegalArgumentException, Unit] =
      fields.traverse(_.get(leaves)).flatMap { values =>
        for
          _ <- Checks.check(values.distinct.size == 1, s"Linked parameter values differ at ${paths.mkString(", ")}")
          _ <- Checks.check(domain.contains(values.head), s"Value outside the domain at ${paths.mkString(", ")}")
        yield ()
      }
    override def restoreFixed(leaves: Leaves): Either[IllegalArgumentException, Leaves] =
      if (active) Right(leaves) else set(leaves, fields.head.base)
    override def sample(leaves: Leaves)(using Random): Either[IllegalArgumentException, Leaves] =
      if (active) set(leaves, domain.sample) else Right(leaves)
    override def mutate(leaves: Leaves, probability: Double)(using Random): Either[IllegalArgumentException, Leaves] =
      get(leaves).flatMap(value => set(leaves, domain.mutateWithProbability(value, probability)))
    override def nearby(leaves: Leaves, radius: Double)(using Random): Either[IllegalArgumentException, Leaves] =
      get(leaves).flatMap(value => set(leaves, domain.nearby(value, radius)))
    override def cross(first: Leaves, second: Leaves)(using Random): Either[IllegalArgumentException, Leaves] =
      for
        a      <- get(first)
        b      <- get(second)
        result <- set(first, domain.cross(a, b))
      yield result
    override def distance(first: Leaves, second: Leaves): Either[IllegalArgumentException, Double] =
      for
        a <- get(first)
        b <- get(second)
      yield domain.distance(a, b)
  }
}
