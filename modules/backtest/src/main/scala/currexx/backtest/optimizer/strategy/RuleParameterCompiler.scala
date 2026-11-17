package currexx.backtest.optimizer.strategy

import cats.syntax.traverse.*
import cats.syntax.foldable.*
import currexx.backtest.optimizer.Checks
import currexx.domain.signal.{Indicator, IndicatorId}
import currexx.core.trade.{Rule, StrategyCheck, TradeStrategy}
import currexx.domain.signal.Direction

import scala.concurrent.duration.*

/** Resolve disjoint targets against the fixed tree and compile one domain per linked coordinate. */
private[strategy] object RuleParameterCompiler {
  import LeafParameter.Field
  import Rule.Condition as C
  import RuleParameterBounds.{DoubleRange, DurationRange}

  final private case class Fields(numeric: Vector[Field[Double]] = Vector.empty, durations: Vector[Field[FiniteDuration]] = Vector.empty):
    def ++(other: Fields): Fields = Fields(numeric ++ other.numeric, durations ++ other.durations)

  def compile(
      template: TradeStrategy,
      bounds: RuleParameterBounds,
      inputs: Map[IndicatorId, Indicator]
  ): Either[IllegalArgumentException, Vector[LeafParameter]] =
    for
      _ <- (bounds.numeric.keys ++ bounds.durations.keys).toVector.sortBy(_.toString).traverse_ {
        case RuleTarget.At(path, expected) =>
          Checks.check(template == expected, s"Stale rule target at $path: the strategy differs from its bound snapshot; rebind the target")
        case _ => Right(())
      }
      leaves <- RuleTree.leaves(template).zipWithIndex.toList.traverse { case ((path, leaf), order) =>
        StrategyCheck
          .validateCondition(leaf, inputs)
          .left
          .map(message => new IllegalArgumentException(s"$message at $path"))
          .flatMap(_ => fields(path, leaf, order))
      }
      available = leaves.foldLeft(Fields())(_ ++ _)
      numericGroups  <- resolve(bounds.numeric, available.numeric, "numeric", numericTarget, value => DoubleRange(value, value, 1.0))
      durationGroups <- resolve(
        bounds.durations,
        available.durations,
        "duration",
        durationTarget,
        value => DurationRange(value, value, 1.nano)
      )
      numeric <- numericGroups.traverse { (fields, range) =>
        val paths = fields.map(_.path).mkString(", ")
        for
          _ <- Checks.check(
            fields.forall(field => field.valid(range.min) && field.valid(range.max)),
            s"Invalid numeric limits at $paths"
          )
          domain <- ParameterDomain.real(range).left.map(error => new IllegalArgumentException(s"${error.getMessage} at $paths", error))
        yield LeafParameter(fields, domain)
      }
      durations <- durationGroups.traverse { (fields, range) =>
        ParameterDomain
          .duration(range)
          .left
          .map(error => new IllegalArgumentException(s"${error.getMessage} at ${fields.map(_.path).mkString(", ")}", error))
          .map(domain => LeafParameter(fields, domain))
      }
    yield (numeric ++ durations).sortBy(_.order)

  private def numericTarget(target: RuleTarget): Boolean = target match
    case RuleTarget.AtrStop                                                                                                  => true
    case RuleTarget.At(RuleParameterPath(_, RuleParameter.Level | RuleParameter.Threshold | RuleParameter.AtrMultiplier), _) => true
    case _                                                                                                                   => false

  private def durationTarget(target: RuleTarget): Boolean = target match
    case RuleTarget.TrendAge | RuleTarget.HoldingTime                   => true
    case RuleTarget.At(RuleParameterPath(_, RuleParameter.Duration), _) => true
    case _                                                              => false

  private def resolve[A, R](
      settings: Map[RuleTarget, R],
      available: Vector[Field[A]],
      kind: String,
      compatible: RuleTarget => Boolean,
      fixed: A => R
  ): Either[IllegalArgumentException, Vector[(Vector[Field[A]], R)]] =
    for
      configured <- settings.toVector
        .sortBy(_._1.toString)
        .traverse { (target, range) =>
          val selected = available.filter(_.matches(target))
          for
            _ <- Checks.check(compatible(target), s"Target $target is not a $kind parameter")
            _ <- Checks.check(
              selected.nonEmpty || !target.isInstanceOf[RuleTarget.At],
              s"Target $target matches no $kind parameters"
            )
          yield selected.groupBy(_.base).values.toVector.sortBy(_.head.order).map(_ -> range)
        }
        .map(_.flatten)
      used        = configured.flatMap(_._1.map(_.path))
      overlapping = used.groupBy(identity).collect { case (path, entries) if entries.size > 1 => path }.toList.sortBy(_.toString)
      _ <- Checks.check(overlapping.isEmpty, s"Overlapping rule targets at ${overlapping.mkString(", ")}")
      fixedFields = available.filterNot(field => used.contains(field.path)).map(field => Vector(field) -> fixed(field.base))
    yield configured ++ fixedFields

  private def fields(path: RulePath, leaf: C, order: Int): Either[IllegalArgumentException, Fields] = {
    def key(field: RuleParameter): RuleParameterPath                         = RuleParameterPath(path, field)
    def moving(direction: Direction): Either[IllegalArgumentException, Unit] =
      Checks.check(
        direction != Direction.Still,
        s"Unreachable Still direction at ${key(RuleParameter.Direction)}"
      )
    def number(field: RuleParameter, value: Double, valid: Double => Boolean, target: Option[RuleTarget] = None)(
        read: PartialFunction[C, Double],
        write: PartialFunction[(C, Double), C]
    ): Fields = Fields(numeric = Vector(Field(key(field), target, order, value, read, write, valid)))
    def duration(value: FiniteDuration, target: RuleTarget)(
        read: PartialFunction[C, FiniteDuration],
        write: PartialFunction[(C, FiniteDuration), C]
    ): Fields = Fields(durations = Vector(Field(key(RuleParameter.Duration), Some(target), order, value.toCoarsest, read, write)))
    leaf match
      case C.TrendChangedTo(d, _)      => moving(d).map(_ => Fields())
      case C.TrendIs(d, _)             => moving(d).map(_ => Fields())
      case C.CrossoverOccurred(d, _)   => moving(d).map(_ => Fields())
      case C.UpperBandCrossed(d, _)    => moving(d).map(_ => Fields())
      case C.LowerBandCrossed(d, _)    => moving(d).map(_ => Fields())
      case C.PriceCrossedLine(_, d, _) => moving(d).map(_ => Fields())
      case C.TrendActiveFor(value, _)  =>
        Right(
          duration(value, RuleTarget.TrendAge)(
            { case C.TrendActiveFor(d, _) => d },
            { case (c: C.TrendActiveFor, d) => c.copy(duration = d) }
          )
        )
      case C.PositionOpenFor(value) =>
        Right(
          duration(value, RuleTarget.HoldingTime)(
            { case C.PositionOpenFor(d) => d },
            { case (_: C.PositionOpenFor, d) => C.PositionOpenFor(d) }
          )
        )
      case C.VelocityCrossedLevel(value, direction, _) =>
        moving(direction).map { _ =>
          number(RuleParameter.Level, value, _ => true)(
            { case C.VelocityCrossedLevel(l, _, _) => l },
            { case (c: C.VelocityCrossedLevel, l) => c.copy(level = l) }
          )
        }
      case C.VelocityIsBelow(value, _) =>
        Right(
          number(RuleParameter.Threshold, value, _ >= 0)(
            { case C.VelocityIsBelow(t, _) => t },
            { case (c: C.VelocityIsBelow, t) => c.copy(threshold = t) }
          )
        )
      case C.ValueIs(_, _, value, _) =>
        Right(
          number(RuleParameter.Level, value, _ => true)(
            { case C.ValueIs(_, _, l, _) => l },
            { case (c: C.ValueIs, l) => c.copy(level = l) }
          )
        )
      case C.PriceMovedAgainstEntry(value, _, _) =>
        Right(
          number(RuleParameter.AtrMultiplier, value, _ > 0, Some(RuleTarget.AtrStop))(
            { case C.PriceMovedAgainstEntry(n, _, _) => n },
            { case (c: C.PriceMovedAgainstEntry, n) => c.copy(nAtr = n) }
          )
        )
      case _: C.MomentumEntered | _: C.MomentumIsIn | _: C.MomentumIs | _: C.PreviousVolatilityIs | _: C.VolatilityIs | _: C.VelocityIs |
          _: C.PositionIs | C.NoPosition =>
        Right(Fields())
      case _: C.AllOf | _: C.AnyOf | _: C.Not => Left(new IllegalArgumentException(s"Expected a leaf at $path"))
  }
}
