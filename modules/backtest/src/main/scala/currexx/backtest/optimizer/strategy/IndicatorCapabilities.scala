package currexx.backtest.optimizer.strategy

import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation}

/** Possible profile writes from the fixed indicator tree. Numeric ranges remain explicit because a role does not determine its units. */
final private[strategy] case class IndicatorCapabilities(
    trend: Boolean,
    crossover: Boolean,
    momentumZone: Boolean,
    volatility: Boolean,
    bands: Boolean,
    lineRoles: Set[ValueRole],
    valueRoles: Set[ValueRole],
    priceStop: Boolean
)

private[strategy] object IndicatorCapabilities:
  def from(indicator: Indicator): IndicatorCapabilities =
    def leaves(value: Indicator): List[Indicator] = value match
      case Indicator.Composite(children, _) => children.toList.flatMap(leaves)
      case leaf                             => List(leaf)
    val all          = leaves(indicator)
    val trackers     = all.collect { case tracker: Indicator.ValueTracking => tracker }
    val hasThreshold = all.exists(_.isInstanceOf[Indicator.ThresholdCrossing])
    val prices       = trackers.filter(_.role == ValueRole.Price)
    val volatility   = trackers.filter(_.role == ValueRole.Volatility)
    IndicatorCapabilities(
      trend = all.exists(_.isInstanceOf[Indicator.TrendChangeDetection]),
      crossover = all.exists(_.isInstanceOf[Indicator.LinesCrossing]),
      momentumZone = hasThreshold,
      volatility = all.exists(_.isInstanceOf[Indicator.VolatilityRegimeDetection]),
      bands = all.exists {
        case _: Indicator.BollingerBands | _: Indicator.KeltnerChannel => true
        case _                                                         => false
      },
      lineRoles = all.collect { case Indicator.PriceLineCrossing(_, role, _) => role }.toSet,
      valueRoles = trackers.map(_.role).toSet ++ Option.when(hasThreshold)(ValueRole.Momentum),
      priceStop = prices.nonEmpty && volatility.nonEmpty && prices.forall { tracker =>
        tracker.source == ValueSource.Close && tracker.transformation == ValueTransformation.SMA(1)
      } && volatility.forall(_.transformation.isInstanceOf[ValueTransformation.ATR])
    )
