package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MomentumOscillators {

  def aroon(values: List[Double], length: Int): List[Double] = {
    val res = Array.ofDim[Double](values.size)
    def calc(remainingValues: List[Double], i: Int, window: Queue[Double]): List[Double] =
      if (remainingValues.isEmpty) res.take(values.size - length).toList
      else if (window.size < length) calc(remainingValues.tail, i - 1, window.enqueue(remainingValues.head))
      else {
        val max  = window.zipWithIndex.maxBy(_._1)
        val min  = window.zipWithIndex.minBy(_._1)
        val up   = 100.0 * (max._2 + 1) / length
        val down = 100.0 * (min._2 + 1) / length
        res(i) = up - down
        calc(remainingValues.tail, i - 1, window.drop(1).enqueue(remainingValues.head))
      }
    calc(values.reverse, values.length - 1, Queue.empty)
  }

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
      length: Int,
      slowKLength: Int,
      slowDLength: Int
  ): (List[Double], List[Double]) = {
    val highsArr    = highs.toArray.reverse
    val lowsArr     = lows.toArray.reverse
    val closingsArr = closings.toArray.reverse
    val stochs      = Array.ofDim[Double](closings.size - length)
    val hh          = Queue.empty[Double]
    val ll          = Queue.empty[Double]
    var i           = 0
    while (i < closings.length) {
      hh.enqueue(highsArr(i))
      ll.enqueue(lowsArr(i))
      if (i > length) {
        stochs(i - length) = (closingsArr(i) - ll.min) / (hh.max - ll.min) * 100d
        hh.dequeue()
        ll.dequeue()
      }
      i += 1
    }
    val k = MovingAverages.simple(stochs.reverse.toList, slowKLength)
    val d = MovingAverages.simple(k, slowDLength)
    (k, d)
  }
}
