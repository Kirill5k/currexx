package currexx.backtest.optimizer

import currexx.domain.market.{ValueTransformation, MovingAverage}

trait Optimisable[T]:
  def toGenome(target: T): Array[Array[Int]]
  def fromGenome(genome: Array[Array[Int]]): T

object Optimisable:
  given Optimisable[ValueTransformation] = new Optimisable[ValueTransformation] {
    extension (i: Int)
      def toBinaryArray(minSize: Int): Array[Int] =
        i.toBinaryString.reverse.padTo(minSize, '0').reverse.map(_.toString.toInt).toArray

    extension (array: Array[Int])
      def toInt: Int       = Integer.parseInt(array.mkString(""), 2)
      def toDouble: Double = array.toInt.toDouble

    override def toGenome(target: ValueTransformation): Array[Array[Int]] =
      target match
        case ValueTransformation.Sequenced(sequence) =>
          sequence.toArray.flatMap(toGenome)
        case kalman @ ValueTransformation.Kalman(gain) =>
          Array(Array(kalman.ordinal), (gain * 100).toInt.toBinaryArray(7))
        case wma @ ValueTransformation.WMA(length) =>
          Array(Array(wma.ordinal), length.toBinaryArray(5))
        case sma @ ValueTransformation.SMA(length) =>
          Array(Array(sma.ordinal), length.toBinaryArray(5))
        case ema @ ValueTransformation.EMA(length) =>
          Array(Array(ema.ordinal), length.toBinaryArray(5))
        case hma @ ValueTransformation.HMA(length) =>
          Array(Array(hma.ordinal), length.toBinaryArray(5))
        case nma @ ValueTransformation.NMA(length, signalLength, lambda, _) =>
          Array(
            Array(nma.ordinal),
            length.toBinaryArray(6),
            signalLength.toBinaryArray(5),
            (lambda * 10).toInt.toBinaryArray(6)
          )
    
    override def fromGenome(genome: Array[Array[Int]]): ValueTransformation = {
      def go(remaining: Array[Array[Int]], transformations: Vector[ValueTransformation]): Vector[ValueTransformation] =
        if (remaining.isEmpty) transformations
        else {
          remaining.head.head match
            case 1 => go(remaining.drop(2), transformations :+ ValueTransformation.Kalman(remaining(1).toDouble / 100))
            case 2 => go(remaining.drop(2), transformations :+ ValueTransformation.WMA(remaining(1).toInt))
            case 3 => go(remaining.drop(2), transformations :+ ValueTransformation.SMA(remaining(1).toInt))
            case 4 => go(remaining.drop(2), transformations :+ ValueTransformation.EMA(remaining(1).toInt))
            case 5 => go(remaining.drop(2), transformations :+ ValueTransformation.HMA(remaining(1).toInt))
            case 6 => 
              val nma = ValueTransformation.NMA(
                remaining(1).toInt,
                remaining(2).toInt,
                remaining(3).toDouble / 10,
                MovingAverage.Weighted
              )
              go(remaining.drop(4), transformations :+ nma)
        }
      ValueTransformation.Sequenced(go(genome, Vector.empty).toList)
    }
  }
