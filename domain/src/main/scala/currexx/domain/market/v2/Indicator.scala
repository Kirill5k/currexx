package currexx.domain.market.v2

import currexx.domain.market.v2
import io.circe.{Codec, Decoder, Encoder, Json}
import io.circe.syntax.*

enum MovingAverage(val kind: String):
  case Weighted    extends MovingAverage("weighted")
  case Exponential extends MovingAverage("exponential")
  case Simple      extends MovingAverage("simple")
object MovingAverage {
  inline given Encoder[MovingAverage] = Encoder[String].contramap(_.kind)
  inline given Decoder[MovingAverage] =
    Decoder[String].emap(ma => MovingAverage.values.find(_.kind == ma).toRight(s"Unrecognized moving average kind $ma"))
}

enum CompositeMovingAverage(val kind: String):
  case Triple  extends CompositeMovingAverage("triple")
  case Hull    extends CompositeMovingAverage("hull")
  case Nyquist extends CompositeMovingAverage("nyquist")
object CompositeMovingAverage {
  inline given Encoder[CompositeMovingAverage] = Encoder[String].contramap(_.kind)
  inline given Decoder[CompositeMovingAverage] =
    Decoder[String].emap(ma => CompositeMovingAverage.values.find(_.kind == ma).toRight(s"Unrecognized moving average kind $ma"))
}

enum ValueSource(val kind: String) {
  case Close extends ValueSource("close")
  case Open  extends ValueSource("open")
}
object ValueSource {
  inline given Encoder[ValueSource] = Encoder[String].contramap(_.kind)
  inline given Decoder[ValueSource] =
    Decoder[String].emap(s => ValueSource.values.find(_.kind == s).toRight(s"Unrecognized value source $s"))
}

sealed trait ValueTransformation[In, Out](val kind: String)
object ValueTransformation {

  final case class EMA(length: Int) extends ValueTransformation[List[Double], List[Double]]("ema")
  final case class HMA(length: Int) extends ValueTransformation[List[Double], List[Double]]("hma")
  final case class NMA(
      length: Int,
      signalLength: Int,
      lambda: Double,
      maCalc: MovingAverage
  ) extends ValueTransformation[List[Double], List[Double]]("nma")

  final case class Sequenced[In, Med, Out](
      transformation1: ValueTransformation[In, Med],
      transformation2: ValueTransformation[Med, Out]
  ) extends ValueTransformation[In, Out]("nma")

  extension [In, Med](transformation1: ValueTransformation[In, Med])
    def ~>[Out](transformation2: ValueTransformation[Med, Out]): ValueTransformation[In, Out] =
      Sequenced(transformation1, transformation2)
}
