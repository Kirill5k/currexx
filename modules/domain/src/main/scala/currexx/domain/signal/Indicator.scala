package currexx.domain.signal

import cats.data.NonEmptyList
import currexx.domain.types.EnumType
import io.circe.derivation.{Configuration, ConfiguredCodec, renaming}

object MovingAverage extends EnumType[MovingAverage](() => MovingAverage.values)
enum MovingAverage:
  case Weighted, Exponential, Simple, Hull

object ValueSource extends EnumType[ValueSource](() => ValueSource.values, EnumType.printLowerCase(_))
enum ValueSource:
  case Close, Open, HL2, HLC3

object ValueRole extends EnumType[ValueRole](() => ValueRole.values)
enum ValueRole:
  case Momentum, Volatility, Velocity, ChannelMiddleBand, TrendStrength, Price

enum ValueTransformation derives ConfiguredCodec:
  case Sequenced(sequence: List[ValueTransformation])
  case StandardDeviation(length: Int)
  case Kalman(gain: Double, measurementNoise: Double)
  case KalmanVelocity(gain: Double, measurementNoise: Double)
  case ATR(length: Int)
  case RSX(length: Int)
  case JRSX(length: Int)
  case WMA(length: Int)
  case SMA(length: Int)
  case EMA(length: Int)
  case HMA(length: Int)
  case NMA(length: Int, signalLength: Int, lambda: Double, maCalc: MovingAverage)
  case JMA(length: Int, phase: Int, power: Int)
  case STOCH(length: Int)
  case ADX(length: Int)
  case WilliamsR(length: Int)
  case CCI(length: Int)
  case IchimokuKijunSen(length: Int)
  case ParabolicSAR(afStart: Double, afMax: Double, afStep: Double)
  case CMF(length: Int)

object ValueTransformation {
  def sequenced(vt: ValueTransformation, vtSequence: ValueTransformation*): ValueTransformation =
    ValueTransformation.Sequenced(vt :: vtSequence.toList)

  private val customMappings: Map[String, String] = Map(
    "StandardDeviation" -> "stdev",
    "IchimokuKijunSen"  -> "ichimoku-kijun"
  )

  given Configuration = Configuration.default
    .withDiscriminator("kind")
    .withTransformConstructorNames(name => customMappings.getOrElse(name, renaming.kebabCase(name)))
}

object CombinationLogic extends EnumType[CombinationLogic](() => CombinationLogic.values)
enum CombinationLogic:
  case All, Any

enum Indicator derives ConfiguredCodec:
  case Composite(
      indicators: NonEmptyList[Indicator],
      combinator: CombinationLogic
  )
  case TrendChangeDetection(
      source: ValueSource,
      transformation: ValueTransformation
  )
  case ThresholdCrossing(
      source: ValueSource,
      transformation: ValueTransformation,
      upperBoundary: Double,
      lowerBoundary: Double
  )
  case LinesCrossing(
      source: ValueSource,
      line1Transformation: ValueTransformation, // SLOW
      line2Transformation: ValueTransformation  // FAST
  )
  case KeltnerChannel(
      source: ValueSource,
      middleBand: ValueTransformation,
      atrLength: Int,
      atrMultiplier: Double
  )
  case VolatilityRegimeDetection(
      atrLength: Int,
      smoothingType: ValueTransformation
  )
  case ValueTracking(
      role: ValueRole,
      source: ValueSource,
      transformation: ValueTransformation
  )
  case PriceLineCrossing(
      source: ValueSource,
      role: ValueRole,
      transformation: ValueTransformation
  )
  case BollingerBands(
      source: ValueSource,
      middleBand: ValueTransformation,
      stdDevLength: Int,
      stdDevMultiplier: Double
  )

object Indicator:
  given Configuration = Configuration.default
    .withDiscriminator("kind")
    .withKebabCaseConstructorNames

  def compositeAllOf(indicator: Indicator, indicators: Indicator*): Indicator =
    Indicator.Composite(NonEmptyList.of(indicator, indicators*), CombinationLogic.All)

  def compositeAnyOf(indicator: Indicator, indicators: Indicator*): Indicator =
    Indicator.Composite(NonEmptyList.of(indicator, indicators*), CombinationLogic.Any)
