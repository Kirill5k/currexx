package currexx.domain.market.v2

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
  case Triple  extends MovingAverage("triple")
  case Hull    extends MovingAverage("hull")
  case Nyquist extends MovingAverage("nyquist")
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
