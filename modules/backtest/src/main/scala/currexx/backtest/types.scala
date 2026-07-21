package currexx.backtest

import eu.timepit.refined.api.{Refined, RefinedTypeOps}
import eu.timepit.refined.numeric.{Greater, Interval}
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal, PosDouble, PosInt}

object types {
  type PositiveUnitInterval = Double Refined Interval.OpenClosed[0.0, 1.0]
  object PositiveUnitInterval extends RefinedTypeOps[PositiveUnitInterval, Double]

  /** Strictly greater than 1.0. */
  type GreaterThanOne = Double Refined Greater[1.0]
  object GreaterThanOne extends RefinedTypeOps[GreaterThanOne, Double]

  given Conversion[Int, PosInt]                  = PosInt.unsafeFrom(_)
  given Conversion[Double, PosDouble]            = PosDouble.unsafeFrom(_)
  given Conversion[Double, PositiveUnitInterval] = PositiveUnitInterval.unsafeFrom(_)
  given Conversion[Double, GreaterThanOne]       = GreaterThanOne.unsafeFrom(_)
  given Conversion[BigDecimal, PosBigDecimal]    = PosBigDecimal.unsafeFrom(_)
  given Conversion[BigDecimal, NonNegBigDecimal] = NonNegBigDecimal.unsafeFrom(_)
}
