package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.{Queue => MQueue}

object MomentumOscillators {

  def relativeStrengthIndex(values: List[Double], length: Int): List[Double] = {
    val rsis = Array.ofDim[Double](values.size)
    @tailrec
    def calc(remainingValues: List[Double], prevValue: Double, i: Int, gain: Double, loss: Double): Array[Double] =
      if (remainingValues.isEmpty) rsis
      else {
        val diff     = remainingValues.head - prevValue
        val currGain = diff.max(0d)
        val currLoss = diff.min(0d).abs
        if (i < length) calc(remainingValues.tail, remainingValues.head, i + 1, gain + currGain, loss + currLoss)
        else {
          val (avgGain, avgLoss) =
            if (i == length) ((gain + currLoss) / length, (loss + currLoss) / length)
            else ((gain * (length - 1) + currGain) / length, (loss * (length - 1) + currLoss) / length)
          val rsi = 100d - (100d / (1 + avgGain / avgLoss))
          rsis(values.length - i - 1) = rsi
          calc(remainingValues.tail, remainingValues.head, i + 1, avgGain, avgLoss)
        }
      }
    val allValues = values.reverse
    calc(allValues.tail, allValues.head, 1, 0d, 0d).take(rsis.length - length).toList
  }

  def stochastic(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val highsArr    = highs.toArray.reverse
    val lowsArr     = lows.toArray.reverse
    val closingsArr = closings.toArray.reverse
    val stochs      = Array.ofDim[Double](closings.size - length)
    val hh          = MQueue.empty[Double]
    val ll          = MQueue.empty[Double]
    var i           = 0
    while (i < closings.length) {
      hh.enqueue(highsArr(i))
      ll.enqueue(lowsArr(i))
      if (i > length) {
        stochs(i - length) = (closingsArr(i) - ll.min) / (hh.max - ll.min) * 100d
        hh.dequeue()
        ll.dequeue()
        ()
      }
      i += 1
    }
    stochs.reverse.toList
  }

  def jurikRelativeStrengthIndex(values: List[Double], length: Int): List[Double] = {
    val f18 = 3.0 / (length + 2)
    val f20 = 1.0 - f18

    @tailrec
    def go(
        remaining: List[Double],
        result: List[Double],
        prevF8: Double,
        prevF28: Double,
        prevF30: Double,
        prevF38: Double,
        prevF40: Double,
        prevF48: Double,
        prevF50: Double,
        prevF58: Double,
        prevF60: Double,
        prevF68: Double,
        prevF70: Double,
        prevF78: Double,
        prevF80: Double,
        prevF88: Double,
        prevF90: Double
    ): List[Double] =
      if (remaining.isEmpty) result
      else {
        val f8  = 100 * remaining.head
        val f10 = prevF8
        val v8  = f8 - f10
        val f28 = f20 * prevF28 + f18 * v8
        val f30 = f18 * f28 + f20 * prevF30
        val vC  = f28 * 1.5 - f30 * 0.5
        val f38 = f20 * prevF38 + f18 * vC
        val f40 = f18 * f38 + f20 * prevF40
        val v10 = f38 * 1.5 - f40 * 0.5
        val f48 = f20 * prevF48 + f18 * v10
        val f50 = f18 * f48 + f20 * prevF50
        val v14 = f48 * 1.5 - f50 * 0.5
        val f58 = f20 * prevF58 + f18 * math.abs(v8)
        val f60 = f18 * f58 + f20 * prevF60
        val v18 = f58 * 1.5 - f60 * 0.5
        val f68 = f20 * prevF68 + f18 * v18
        val f70 = f18 * f68 + f20 * prevF70
        val v1C = f68 * 1.5 - f70 * 0.5
        val f78 = f20 * prevF78 + f18 * v1C
        val f80 = f18 * f78 + f20 * prevF80
        val v20 = f78 * 1.5 - f80 * 0.5
        val f90 = if (prevF90 == 0) 1 else if (prevF88 <= prevF90) prevF88 + 1 else prevF90 + 1
        val f88 = if (prevF90 == 0 && (length - 1) >= 5) length - 1 else 5
        val f0  = if (f88 >= f90 && f8 != f10) 1 else 0
        val f91 = if (f88 == f90 && f0 == 0) 0 else f90
        val v4  = if (f88 < f91 && v20 > 0) (v14 / v20 + 1) * 50 else 50
        val rsx = if (v4 > 100) 100 else if (v4 < 0) 0 else v4
        go(
          remaining.tail,
          rsx :: result,
          f8,
          f28,
          f30,
          f38,
          f40,
          f48,
          f50,
          f58,
          f60,
          f68,
          f70,
          f78,
          f80,
          f88,
          f90
        )
      }
    go(values.reverse, Nil, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  }
}
