package currexx.backtest

import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.numeric.{Greater, GreaterEqual, Interval}
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.{NonNegBigDecimal, PosBigDecimal, PosDouble, PosInt}

object types {
  type NonEmptyMap[K, V] = Map[K, V] Refined NonEmpty
  object NonEmptyMap {
    def from[K, V](value: Map[K, V]): Either[String, NonEmptyMap[K, V]] = refineV[NonEmpty](value)
  }

  final case class FiniteNonNegative()
  object FiniteNonNegative {
    given Validate[Double, FiniteNonNegative] = Validate.fromPredicate(
      value => value.isFinite && value >= 0,
      value => s"$value must be finite and non-negative",
      FiniteNonNegative()
    )
  }
  type FiniteNonNegDouble = Double Refined FiniteNonNegative
  object FiniteNonNegDouble extends RefinedTypeOps[FiniteNonNegDouble, Double]

  type PositiveUnitInterval = Double Refined Interval.OpenClosed[0.0, 1.0]
  object PositiveUnitInterval extends RefinedTypeOps[PositiveUnitInterval, Double]

  type OpenUnitInterval = Double Refined Interval.Open[0.0, 1.0]
  object OpenUnitInterval extends RefinedTypeOps[OpenUnitInterval, Double]

  type AtLeastOneBigDecimal = BigDecimal Refined GreaterEqual[1]
  object AtLeastOneBigDecimal extends RefinedTypeOps[AtLeastOneBigDecimal, BigDecimal]

  /** Strictly greater than 1.0. */
  type GreaterThanOne = Double Refined Greater[1.0]
  object GreaterThanOne extends RefinedTypeOps[GreaterThanOne, Double]

  given Conversion[Int, PosInt]                      = PosInt.unsafeFrom(_)
  given Conversion[Double, PosDouble]                = PosDouble.unsafeFrom(_)
  given Conversion[Double, PositiveUnitInterval]     = PositiveUnitInterval.unsafeFrom(_)
  given Conversion[Double, OpenUnitInterval]         = OpenUnitInterval.unsafeFrom(_)
  given Conversion[Double, GreaterThanOne]           = GreaterThanOne.unsafeFrom(_)
  given Conversion[BigDecimal, PosBigDecimal]        = PosBigDecimal.unsafeFrom(_)
  given Conversion[BigDecimal, NonNegBigDecimal]     = NonNegBigDecimal.unsafeFrom(_)
  given Conversion[BigDecimal, AtLeastOneBigDecimal] = AtLeastOneBigDecimal.unsafeFrom(_)
}
