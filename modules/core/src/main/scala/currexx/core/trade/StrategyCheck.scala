package currexx.core.trade

import currexx.domain.signal.{Indicator, IndicatorId, ValueRole, ValueSource, ValueTransformation}

/** The same input contract is used for live activation, rule readiness and optimiser dependencies. */
object StrategyCheck:
  private val validId = "[a-z0-9-]+".r

  enum Capability:
    case Trend, Zone, Regime, Crossover, Bands
    case Line(role: ValueRole)
    case Value(role: ValueRole)
    case RawClose, Atr

  enum Timing:
    case State, History, Event

  final case class Input(id: IndicatorId, capability: Capability, timing: Timing = Timing.State)

  def inputs(condition: Rule.Condition): List[Input] =
    import Rule.Condition as C
    def input(of: Option[IndicatorId], default: IndicatorId, capability: Capability, timing: Timing = Timing.State) =
      List(Input(of.getOrElse(default), capability, timing))
    condition match
      case C.AllOf(cs)                => cs.flatMap(inputs)
      case C.AnyOf(cs)                => cs.flatMap(inputs)
      case C.Not(c)                   => inputs(c)
      case C.TrendIs(_, of)           => input(of, IndicatorId.Trend, Capability.Trend)
      case C.TrendActiveFor(_, of)    => input(of, IndicatorId.Trend, Capability.Trend)
      case C.TrendChangedTo(_, of)    => input(of, IndicatorId.Trend, Capability.Trend, Timing.Event)
      case C.CrossoverOccurred(_, of) => input(of, IndicatorId.Crossover, Capability.Crossover, Timing.Event)
      case C.MomentumEntered(_, of)   => input(of, IndicatorId.MomentumZone, Capability.Zone, Timing.Event)
      case C.MomentumIsIn(_, of)      => input(of, IndicatorId.MomentumZone, Capability.Zone)
      case C.MomentumIs(_, of) => input(of, IndicatorId.forValue(ValueRole.Momentum), Capability.Value(ValueRole.Momentum), Timing.History)
      case C.PreviousVolatilityIs(_, of)    => input(of, IndicatorId.VolatilityRegime, Capability.Regime, Timing.History)
      case C.VolatilityIs(_, of)            => input(of, IndicatorId.VolatilityRegime, Capability.Regime)
      case C.VelocityIs(_, of)              => input(of, IndicatorId.forValue(ValueRole.Velocity), Capability.Value(ValueRole.Velocity))
      case C.VelocityIsBelow(_, of)         => input(of, IndicatorId.forValue(ValueRole.Velocity), Capability.Value(ValueRole.Velocity))
      case C.VelocityCrossedLevel(_, _, of) =>
        input(of, IndicatorId.forValue(ValueRole.Velocity), Capability.Value(ValueRole.Velocity), Timing.History)
      case C.UpperBandCrossed(_, of)               => input(of, IndicatorId.Band, Capability.Bands, Timing.Event)
      case C.LowerBandCrossed(_, of)               => input(of, IndicatorId.Band, Capability.Bands, Timing.Event)
      case C.PriceCrossedLine(role, _, of)         => input(of, IndicatorId.forLine(role), Capability.Line(role), Timing.Event)
      case C.ValueIs(role, _, _, of)               => input(of, IndicatorId.forValue(role), Capability.Value(role))
      case C.PriceMovedAgainstEntry(_, price, atr) =>
        input(price, IndicatorId.forValue(ValueRole.Price), Capability.RawClose) ++ input(
          atr,
          IndicatorId.forValue(ValueRole.Volatility),
          Capability.Atr
        )
      case C.NoPosition | _: C.PositionIs | _: C.PositionOpenFor => Nil

  def supports(indicator: Indicator, capability: Capability): Boolean = (indicator.definition, capability) match
    case (_: Indicator.TrendChangeDetection, Capability.Trend)                         => true
    case (_: Indicator.ThresholdCrossing, Capability.Zone)                             => true
    case (_: Indicator.VolatilityRegimeDetection, Capability.Regime)                   => true
    case (_: Indicator.LinesCrossing, Capability.Crossover)                            => true
    case (_: Indicator.KeltnerChannel | _: Indicator.BollingerBands, Capability.Bands) => true
    case (i: Indicator.PriceLineCrossing, Capability.Line(role))                       => i.role == role
    case (i: Indicator.ValueTracking, Capability.Value(role))                          => i.role == role
    case (i: Indicator.ValueTracking, Capability.RawClose)                             =>
      i.role == ValueRole.Price && i.source == ValueSource.Close && i.transformation == ValueTransformation.SMA(1)
    case (i: Indicator.ValueTracking, Capability.Atr) =>
      i.role == ValueRole.Volatility && i.transformation.isInstanceOf[ValueTransformation.ATR]
    case _ => false

  def validateCondition(condition: Rule.Condition, sources: Map[IndicatorId, Indicator]): Either[String, Unit] =
    inputs(condition).find(i => !sources.get(i.id).exists(supports(_, i.capability))) match
      case Some(i) => Left(s"Missing or incompatible ${i.capability} source '${i.id.value}'")
      case None    => Right(())

  def validate(indicators: List[Indicator], strategy: Option[TradeStrategy]): Either[String, Map[IndicatorId, Indicator]] =
    val leaves         = indicators.flatMap(_.leaves)
    val pairs          = leaves.flatMap(i => i.resolvedId.map(_ -> i))
    val duplicate      = pairs.groupMap(_._1)(_._2).collectFirst { case (id, values) if values.size > 1 => id }
    val invalidWrapper = leaves.exists {
      case Indicator.Named(_, _: Indicator.Composite | _: Indicator.Named) => true
      case _                                                               => false
    }
    val invalid = pairs.find { (id, i) =>
      !validId.matches(id.value) || (IndicatorId.reserved.contains(id) && !IndicatorId.defaultFor(i).contains(id))
    }
    for
      _ <- Either.cond(!invalidWrapper, (), "Named indicators must wrap a single unnamed leaf")
      _ <- duplicate.toLeft(()).left.map(id => s"Duplicate indicator id '${id.value}'; name each source explicitly")
      _ <- invalid.toLeft(()).left.map((id, _) => s"Invalid or reserved indicator id '${id.value}'")
      sources = pairs.toMap
      _ <- strategy.toList.flatMap(s => s.openRules ++ s.closeRules).foldLeft[Either[String, Unit]](Right(())) { (result, rule) =>
        result.flatMap(_ => validateCondition(rule.conditions, sources))
      }
    yield sources
