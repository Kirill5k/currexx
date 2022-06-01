package currexx.domain.market.v2

import currexx.domain.market.v2
import org.latestbit.circe.adt.codec.*

enum MovingAverage(val kind: String):
  case Weighted    extends MovingAverage("weighted")
  case Exponential extends MovingAverage("exponential")
  case Simple      extends MovingAverage("simple")

enum CompositeMovingAverage(val kind: String):
  case Triple  extends CompositeMovingAverage("triple")
  case Hull    extends CompositeMovingAverage("hull")
  case Nyquist extends CompositeMovingAverage("nyquist")

enum ValueSource[T](val kind: String):
  case Close extends ValueSource[List[Double]]("close")
  case Open  extends ValueSource[List[Double]]("open")

enum ValueTransformation[In, Out](val kind: String):
  case EMA(length: Int) extends ValueTransformation[List[Double], List[Double]]("ema")
  case HMA(length: Int) extends ValueTransformation[List[Double], List[Double]]("hma")
  case NMA(
      length: Int,
      signalLength: Int,
      lambda: Double,
      maCalc: MovingAverage
  ) extends ValueTransformation[List[Double], List[Double]]("nma")
  case Sequenced[In, Med, Out](
      transformation1: ValueTransformation[In, Med],
      transformation2: ValueTransformation[Med, Out]
  ) extends ValueTransformation[In, Out]("sequenced")

object ValueTransformation {
  extension [In, Med](transformation1: ValueTransformation[In, Med])
    def ~>[Out](transformation2: ValueTransformation[Med, Out]): ValueTransformation[In, Out] =
      Sequenced(transformation1, transformation2)
}

enum Indicator[In, Out](val kind: String):
  case TrendDirectionChange(
      source: ValueSource[List[Double]],
      transformation: ValueTransformation[List[Double], List[Double]]
  ) extends Indicator[List[Double], List[Double]]("trend-direction-change")
