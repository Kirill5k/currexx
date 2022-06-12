package currexx.backtest.optimizer

import currexx.domain.market.ValueTransformation

trait Optimisable[T]:
  def toGenome(target: T): Array[Array[Int]]
  def fromGenome(genome: Array[Array[Int]]): T

object Optimisable:
  given Optimisable[ValueTransformation] = new Optimisable[ValueTransformation] {
    extension (i: Int) def toBinaryArray(size: Int): Array[Int] =
      i.toBinaryString.reverse.padTo(size, '0').reverse.map(_.toString.toInt).toArray

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
        case nma @ ValueTransformation.NMA(length, signalLength, lambda, maCalc) =>
          Array(Array(nma.ordinal), length.toBinaryArray(6), signalLength.toBinaryArray(5), (lambda * 10).toInt.toBinaryArray(6), maCalc.ordinal.toBinaryArray(2))

    override def fromGenome(genome: Array[Array[Int]]): ValueTransformation = ???
  }
