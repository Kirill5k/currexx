package currexx.calculations

import scala.annotation.tailrec
import scala.collection.mutable.Queue

object MovingAverages {

  private val EmaSmoothing: Double = 2.0d

  private def exponentialAsArray(values: List[Double], n: Int, smoothing: Double = EmaSmoothing): Array[Double] = {
    val k         = smoothing / (1 + n)
    val allValues = values.reverse
    val emas      = Array.ofDim[Double](allValues.size)
    emas(allValues.size - 1) = allValues.head
    @tailrec
    def calc(remainingValues: List[Double], i: Int): Array[Double] =
      if (remainingValues.isEmpty) emas
      else {
        val ema = remainingValues.head * k + emas(i) * (1 - k)
        emas.update(i - 1, ema)
        calc(remainingValues.tail, i - 1)
      }
    calc(allValues.tail, allValues.size - 1)
  }

  def exponential(values: List[Double], n: Int, smoothing: Double = EmaSmoothing): List[Double] =
    exponentialAsArray(values, n, smoothing).toList

  def simple(values: List[Double], n: Int): List[Double] = {
    @tailrec
    def calc(queue: Queue[Double], remaining: List[Double], result: List[Double]): List[Double] =
      if (remaining.isEmpty) result
      else if (queue.size < n) calc(queue.addOne(remaining.head), remaining.tail, result)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        val sma          = updatedQueue.sum / n
        calc(updatedQueue, remaining.tail, sma :: result)
      }
    calc(Queue.empty, values.reverse, Nil)
  }

  def macd(values: List[Double], fastLength: Int = 12, slowLength: Int = 26): List[Double] = {
    val fastMa = exponentialAsArray(values, fastLength)
    val slowMa = exponentialAsArray(values, slowLength)
    val macd   = Array.ofDim[Double](values.size)
    var i      = 0
    while (i < values.size) {
      macd(i) = fastMa(i) - slowMa(i)
      i += 1
    }
    macd.toList
  }

  def macdWithSignal(
      values: List[Double],
      fastLength: Int = 12,
      slowLength: Int = 26,
      signalSmoothing: Int = 9
  ): (List[Double], List[Double]) = {
    val macdLine   = macd(values, fastLength, slowLength)
    val signalLine = simple(macdLine, signalSmoothing)
    (macdLine, signalLine)
  }

  def weightedAsArray(values: List[Double], n: Int): Array[Double] = {
    val wmas    = Array.ofDim[Double](values.size)
    val divider = (n * (n + 1)) / 2
    @tailrec
    def calc(queue: Queue[Double], remaining: List[Double], i: Int): Array[Double] =
      if (remaining.isEmpty) wmas.drop(i)
      else if (queue.size < n) calc(queue.addOne(remaining.head), remaining.tail, i)
      else {
        val updatedQueue = queue.drop(1).addOne(remaining.head)
        wmas(i - 1) = updatedQueue.zipWithIndex.foldLeft(0d) { case (sum, (v, i)) => sum + (n + i + 1 - n) * v } / divider
        calc(updatedQueue, remaining.tail, i - 1)
      }
    calc(Queue.empty, values.reverse, values.size)
  }

  def weighted(values: List[Double], n: Int): List[Double] =
    weightedAsArray(values, n).toList

  def hull(values: List[Double], n: Int): List[Double] = {
    val n2    = math.round(n.toDouble / 2).toInt
    val sqrtn   = math.round(math.sqrt(n.toDouble)).toInt
    val nwma  = weightedAsArray(values, n)
    val n2wma = weightedAsArray(values, n2)
    var i     = 0
    val diff  = Array.ofDim[Double](nwma.length)
    while (i < nwma.length) {
      diff(i) = n2wma(i) * 2 - nwma(i)
      i = i + 1
    }
    weighted(diff.toList, sqrtn)
  }

  def triple(
      values: List[Double],
      n: Int,
      maCalc: (List[Double], Int) => List[Double] = (values, n) => exponential(values, n)
  ): List[Double] = {
    val ma1 = maCalc(values, n)
    val ma2 = maCalc(ma1, n)
    val me3 = maCalc(ma2, n)
    ma1.zip(ma2).zip(me3).map { case ((v1, v2), v3) => (3 * v1) - (3 * v2) + v3 }
  }

  def nyquist(
      values: List[Double],
      n1: Int,
      n2: Int,
      lambda: Double,
      maCalc: (List[Double], Int) => List[Double] = (values, n) => weighted(values, n)
  ): List[Double] = {
    val alpha = lambda * (n1 - 1) / (n1 - lambda)
    val nwma1 = maCalc(values, n1)
    val nwma2 = maCalc(nwma1, n2)
    nwma1.zip(nwma2).map { case (v1, v2) => (1 + alpha) * v1 - alpha * v2 }
  }

  def jurik(
      values: List[Double],
      length: Int,
      phase: Int,
      power: Int
  ): List[Double] = {
    // periodic ratio
    val beta = 0.45 * (length - 1) / (0.45 * (length - 1) + 2)
    // phase ratio
    val pr = if (phase < -100) 0.5 else if (phase > 100) 2.5 else phase / 100 + 1.5
    // additional periodic factor
    val len1  = math.max(math.log(math.sqrt(length)) / math.log(2.0) + 2, 0d)
    val pow1  = math.max(len1 - 2, 0.5)
    val alpha = ???
    ???
  }

  def jurikSimplified(
      values: List[Double],
      length: Int,
      phase: Int,
      power: Int
  ): List[Double] = {
    // periodic ratio
    val beta = 0.45 * (length - 1) / (0.45 * (length - 1) + 2)
    // phase ratio
    val pr = if (phase < -100) 0.5 else if (phase > 100) 2.5 else phase / 100 + 1.5
    // dynamic factor
    val alpha = math.pow(beta, power)

    @tailrec
    def go(
        remaining: List[Double],
        result: List[Double],
        prevMa1: Double,
        prevDet0: Double,
        prevDet1: Double,
        prevJma: Double
    ): List[Double] =
      if (remaining.isEmpty) result
      else {
        val price = remaining.head
        val ma1   = (1 - alpha) * price + alpha * prevMa1
        val det0  = (price - ma1) * (1 - beta) + beta * prevDet0
        val ma2   = ma1 + pr * det0
        val det1  = (ma2 - prevJma) * math.pow(1 - alpha, 2) + math.pow(alpha, 2) * prevDet1
        val jma   = prevJma + det1
        go(remaining.tail, jma :: result, ma1, det0, det1, jma)
      }
    go(values.reverse, Nil, 0d, 0d, 0d, 0d)
  }
}
