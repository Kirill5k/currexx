package currexx.backtest.optimizer

import cats.data.NonEmptyList
import cats.syntax.traverse.*
import currexx.backtest.TestStrategy
import currexx.core.trade.Rule
import currexx.domain.signal.{CombinationLogic, Indicator, ValueRole, ValueSource, ValueTransformation as VT}

/** A round's searchable leaves, with fixed indicators retained in the evaluated strategy.
  *
  * Even an unread value tracker can affect profile updates and Composite.All gating, so fixed leaves are restored rather than removed.
  * Every exact occurrence of an explicitly fixed value is frozen; values absent from the template are reported as validation errors.
  */
final class IndicatorSearchSpace private (
    val template: Indicator,
    val fixedIndicators: Set[Indicator],
    private val fixedPaths: Set[Vector[Int]]
) {
  import IndicatorSearchSpace.sameSchema

  private def activeLeaves(indicator: Indicator, path: Vector[Int]): List[(Vector[Int], Indicator)] =
    if (fixedPaths.contains(path)) Nil
    else
      indicator match
        case Indicator.Composite(children, _) =>
          children.toList.zipWithIndex.flatMap((child, index) => activeLeaves(child, path :+ index))
        case leaf => List(path -> leaf)

  private val activeTemplate = activeLeaves(template, Vector.empty)

  /** Compatibility is stricter than crossover: sources, roles, combinators and transformation kinds are part of the strategy. */
  def accepts(candidate: Indicator): Boolean = sameSchema(template, candidate)

  def canonicalise(candidate: Indicator): Either[Throwable, Indicator] = {
    def restoreFixed(original: Indicator, current: Indicator, path: Vector[Int]): Indicator =
      if (fixedPaths.contains(path)) original
      else
        (original, current) match
          case (Indicator.Composite(originals, combinator), Indicator.Composite(currents, _)) =>
            val children = originals.zip(currents).zipWithIndex.map { case ((base, changed), index) =>
              restoreFixed(base, changed, path :+ index)
            }
            Indicator.Composite(children, combinator)
          case _ => current
    Either.cond(
      accepts(candidate),
      restoreFixed(template, candidate, Vector.empty),
      new IllegalArgumentException("Indicator does not match this round's search-space schema")
    )
  }

  /** A flat chromosome keeps fixed leaves out of random draws, mutation, crossover and their bounds repair. */
  def project(candidate: Indicator): Either[Throwable, Option[Indicator]] =
    canonicalise(candidate).map { canonical =>
      NonEmptyList
        .fromList(activeLeaves(canonical, Vector.empty).map(_._2))
        .map(Indicator.Composite(_, CombinationLogic.Any))
    }

  /** Project compatible extra seeds, excluding the target's genes and duplicate projections. */
  def projectSeeds(extraSeeds: List[Indicator]): Either[Throwable, List[Indicator]] =
    for
      target    <- project(template)
      projected <- extraSeeds.filter(accepts).traverse(project)
    yield projected.flatten.filterNot(target.contains).distinct

  /** Rebuild the full strategy before scoring or reporting; gene values are not repaired here. */
  def restore(genes: Option[Indicator]): Either[Throwable, Indicator] = {
    val parsed: Either[IllegalArgumentException, List[Indicator]] = genes match
      case None                                                      => Right(Nil)
      case Some(Indicator.Composite(children, CombinationLogic.Any)) => Right(children.toList)
      case _ => Left(new IllegalArgumentException("Search genes must be a flat Any composite, or None for an entirely fixed strategy"))
    parsed.flatMap { leaves =>
      if (leaves.size != activeTemplate.size)
        Left(new IllegalArgumentException("Search genes do not match the number of active indicator leaves"))
      else if (!activeTemplate.zip(leaves).forall { case ((_, base), leaf) => sameSchema(base, leaf) })
        Left(new IllegalArgumentException("Search genes have incompatible schemas"))
      else {
        val replacements = activeTemplate.map(_._1).zip(leaves).toMap
        def rebuild(original: Indicator, path: Vector[Int]): Either[IllegalArgumentException, Indicator] =
          if (fixedPaths.contains(path)) Right(original)
          else
            original match
              case Indicator.Composite(children, combinator) =>
                children.zipWithIndex.traverse((child, index) => rebuild(child, path :+ index)).map(Indicator.Composite(_, combinator))
              case _ =>
                replacements.get(path).toRight(new IllegalArgumentException("Missing active indicator while restoring search genes"))
        rebuild(template, Vector.empty)
      }
    }
  }
}

object IndicatorSearchSpace {
  def forStrategy(
      strategy: TestStrategy,
      fixedIndicators: Set[Indicator] = Set.empty
  ): Either[IllegalArgumentException, IndicatorSearchSpace] = {
    def nodes(indicator: Indicator, path: Vector[Int]): List[(Vector[Int], Indicator)] =
      (path -> indicator) :: (indicator match
        case Indicator.Composite(children, _) => children.toList.zipWithIndex.flatMap((child, index) => nodes(child, path :+ index))
        case _                                => Nil)
    val allNodes   = nodes(strategy.indicator, Vector.empty)
    val indicators = allNodes.map(_._2).toSet
    val unknown    = fixedIndicators.diff(indicators)
    if (unknown.nonEmpty)
      Left(new IllegalArgumentException(s"Unknown fixed indicators: ${unknown.toList.map(_.toString).sorted.mkString(", ")}"))
    else {
      val readRoles = (strategy.rules.openRules ++ strategy.rules.closeRules).flatMap(rule => trackedRoles(rule.conditions)).toSet
      val automatic: Set[Indicator] = allNodes.collect {
        case (_, tracker @ Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))) => tracker
        case (_, tracker @ Indicator.ValueTracking(role, _, _)) if !readRoles.contains(role)       => tracker
      }.toSet
      val fixed      = fixedIndicators ++ automatic
      val fixedPaths = allNodes.collect { case (path, indicator) if fixed.contains(indicator) => path }.toSet
      Right(new IndicatorSearchSpace(strategy.indicator, fixed, fixedPaths))
    }
  }

  // Keep exhaustive: new rule conditions must declare which tracked values they read before the optimiser can freeze unused roles.
  private def trackedRoles(condition: Rule.Condition): Set[ValueRole] = condition match
    case Rule.Condition.AllOf(conditions)          => conditions.flatMap(trackedRoles).toSet
    case Rule.Condition.AnyOf(conditions)          => conditions.flatMap(trackedRoles).toSet
    case Rule.Condition.Not(inner)                 => trackedRoles(inner)
    case Rule.Condition.ValueIs(role, _, _)        => Set(role)
    case Rule.Condition.MomentumIs(_)              => Set(ValueRole.Momentum)
    case Rule.Condition.VelocityIs(_)              => Set(ValueRole.Velocity)
    case Rule.Condition.VelocityIsBelow(_)         => Set(ValueRole.Velocity)
    case Rule.Condition.VelocityCrossedLevel(_, _) => Set(ValueRole.Velocity)
    case Rule.Condition.PriceMovedAgainstEntry(_)  => Set(ValueRole.Price, ValueRole.Volatility)
    case Rule.Condition.TrendChangedTo(_)          => Set.empty
    case Rule.Condition.TrendIs(_)                 => Set.empty
    case Rule.Condition.TrendActiveFor(_)          => Set.empty
    case Rule.Condition.CrossoverOccurred(_)       => Set.empty
    case Rule.Condition.MomentumEntered(_)         => Set.empty
    case Rule.Condition.MomentumIsIn(_)            => Set.empty
    case Rule.Condition.PreviousVolatilityIs(_)    => Set.empty
    case Rule.Condition.VolatilityIs(_)            => Set.empty
    case Rule.Condition.PositionIs(_)              => Set.empty
    case Rule.Condition.PositionOpenFor(_)         => Set.empty
    case Rule.Condition.NoPosition                 => Set.empty
    case Rule.Condition.UpperBandCrossed(_)        => Set.empty
    case Rule.Condition.LowerBandCrossed(_)        => Set.empty
    case Rule.Condition.PriceCrossedLine(_, _)     => Set.empty

  private def sameSchema(a: Indicator, b: Indicator): Boolean = (a, b) match
    case (Indicator.Composite(as, ac), Indicator.Composite(bs, bc)) =>
      ac == bc && as.length == bs.length && as.toList.zip(bs.toList).forall(sameSchema)
    case (Indicator.TrendChangeDetection(as, at), Indicator.TrendChangeDetection(bs, bt))       => as == bs && sameTransformation(at, bt)
    case (Indicator.ThresholdCrossing(as, at, _, _), Indicator.ThresholdCrossing(bs, bt, _, _)) => as == bs && sameTransformation(at, bt)
    case (Indicator.LinesCrossing(as, a1, a2), Indicator.LinesCrossing(bs, b1, b2))             =>
      as == bs && sameTransformation(a1, b1) && sameTransformation(a2, b2)
    case (Indicator.KeltnerChannel(as, at, _, _), Indicator.KeltnerChannel(bs, bt, _, _))         => as == bs && sameTransformation(at, bt)
    case (Indicator.BollingerBands(as, at, _, _), Indicator.BollingerBands(bs, bt, _, _))         => as == bs && sameTransformation(at, bt)
    case (Indicator.VolatilityRegimeDetection(_, at), Indicator.VolatilityRegimeDetection(_, bt)) => sameTransformation(at, bt)
    case (Indicator.ValueTracking(ar, as, at), Indicator.ValueTracking(br, bs, bt)) => ar == br && as == bs && sameTransformation(at, bt)
    case (Indicator.PriceLineCrossing(as, ar, at), Indicator.PriceLineCrossing(bs, br, bt)) =>
      ar == br && as == bs && sameTransformation(at, bt)
    case _ => false

  private def sameTransformation(a: VT, b: VT): Boolean = (a, b) match
    case (VT.Sequenced(as), VT.Sequenced(bs))               => as.size == bs.size && as.zip(bs).forall(sameTransformation)
    case (VT.NMA(_, _, _, am), VT.NMA(_, _, _, bm))         => am == bm
    case (_: VT.StandardDeviation, _: VT.StandardDeviation) => true
    case (_: VT.Kalman, _: VT.Kalman)                       => true
    case (_: VT.KalmanVelocity, _: VT.KalmanVelocity)       => true
    case (_: VT.ATR, _: VT.ATR)                             => true
    case (_: VT.RSX, _: VT.RSX)                             => true
    case (_: VT.JRSX, _: VT.JRSX)                           => true
    case (_: VT.WMA, _: VT.WMA)                             => true
    case (_: VT.SMA, _: VT.SMA)                             => true
    case (_: VT.EMA, _: VT.EMA)                             => true
    case (_: VT.HMA, _: VT.HMA)                             => true
    case (_: VT.JMA, _: VT.JMA)                             => true
    case (_: VT.STOCH, _: VT.STOCH)                         => true
    case (_: VT.ADX, _: VT.ADX)                             => true
    case (_: VT.WilliamsR, _: VT.WilliamsR)                 => true
    case (_: VT.CCI, _: VT.CCI)                             => true
    case (_: VT.IchimokuKijunSen, _: VT.IchimokuKijunSen)   => true
    case (_: VT.ParabolicSAR, _: VT.ParabolicSAR)           => true
    case (_: VT.CMF, _: VT.CMF)                             => true
    case _                                                  => false
}
