package currexx.backtest.optimizer

import currexx.domain.market.{MovingAverage, ValueTransformation}

trait Optimisable[T]:
  def toGenome(target: T): Array[Array[Int]]
  def fromGenome(genome: Array[Array[Int]]): T

object Optimisable:
  def apply[T](using ev: Optimisable[T]): Optimisable[T] = ev

  given Optimisable[ValueTransformation] = new Optimisable[ValueTransformation] {
    extension (i: Int)
      def divBy(n: Int): Double =
        i.toDouble / n
      def toBinaryArray(minSize: Int): Array[Int] =
        i.toBinaryString.reverse.padTo(minSize, '0').reverse.map(_.toString.toInt).toArray

    extension (array: Array[Int])
      def toInt: Int       = Integer.parseInt(array.mkString(""), 2)
      def toDouble: Double = toInt.toDouble

    override def toGenome(target: ValueTransformation): Array[Array[Int]] =
      target match
        case ValueTransformation.Sequenced(sequence) =>
          sequence.flatMap(toGenome).toArray
        case kalman @ ValueTransformation.Kalman(gain) =>
          Array(Array(kalman.ordinal), (gain * 100).toInt.toBinaryArray(7))
        case wma @ ValueTransformation.WMA(length) =>
          Array(Array(wma.ordinal), length.toBinaryArray(6))
        case sma @ ValueTransformation.SMA(length) =>
          Array(Array(sma.ordinal), length.toBinaryArray(6))
        case ema @ ValueTransformation.EMA(length) =>
          Array(Array(ema.ordinal), length.toBinaryArray(6))
        case hma @ ValueTransformation.HMA(length) =>
          Array(Array(hma.ordinal), length.toBinaryArray(6))
        case nma @ ValueTransformation.NMA(length, signalLength, lambda, _) =>
          Array(
            Array(nma.ordinal),
            length.toBinaryArray(6),
            signalLength.toBinaryArray(5),
            lambda.toInt.toBinaryArray(6)
          )

    override def fromGenome(genome: Array[Array[Int]]): ValueTransformation = {
      def go(remaining: Array[Array[Int]], transformations: Vector[ValueTransformation]): Vector[ValueTransformation] =
        if (remaining.isEmpty) transformations
        else {
          remaining.head.head match
            case 1 => go(remaining.drop(2), transformations :+ ValueTransformation.Kalman(remaining(1).toInt.divBy(100)))
            case 2 => go(remaining.drop(2), transformations :+ ValueTransformation.WMA(remaining(1).toInt))
            case 3 => go(remaining.drop(2), transformations :+ ValueTransformation.SMA(remaining(1).toInt))
            case 4 => go(remaining.drop(2), transformations :+ ValueTransformation.EMA(remaining(1).toInt))
            case 5 => go(remaining.drop(2), transformations :+ ValueTransformation.HMA(remaining(1).toInt))
            case 6 =>
              val nma = ValueTransformation.NMA(
                remaining(1).toInt,
                remaining(2).toInt,
                remaining(3).toDouble,
                MovingAverage.Weighted
              )
              go(remaining.drop(4), transformations :+ nma)
        }
      ValueTransformation.Sequenced(go(genome, Vector.empty).toList)
    }
  }
