package currexx.calculations

import scala.annotation.tailrec

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
    val window = collection.mutable.Queue.empty[Double]
    @tailrec
    def calc(remaining: List[Double], result: List[Double]): List[Double] =
      if (remaining.isEmpty) result
      else if (window.size < n) {
        window.enqueue(remaining.head)
        calc(remaining.tail, result)
      } else {
        val _ = window.dequeue()
        window.enqueue(remaining.head)
        val sma = window.sum / n
        calc(remaining.tail, sma :: result)
      }
    calc(values.reverse, Nil)
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

  private def weightedAsArray(values: Iterator[Double], n: Int, size: Int): Array[Double] = {
    val wmas    = Array.ofDim[Double](size - n)
    val divider = (n * (n + 1)) / 2
    val window  = collection.mutable.Queue.empty[Double]
    var i       = size - n
    while (values.hasNext)
      if (window.size < n) {
        window.enqueue(values.next())
      } else {
        val _ = window.dequeue()
        window.enqueue(values.next())
        wmas(i - 1) = window.zipWithIndex.foldLeft(0d) { case (sum, (v, i)) => sum + (n + i + 1 - n) * v } / divider
        i = i - 1
      }
    wmas
  }

  def weighted(values: List[Double], n: Int): List[Double] =
    weightedAsArray(values.reverseIterator, n, values.size).toList

  def hull(values: List[Double], n: Int): List[Double] = {
    val reversed = values.reverse
    val nwma     = weightedAsArray(reversed.iterator, n, values.size)
    val n2       = math.round(n.toDouble / 2).toInt
    val n2wma    = weightedAsArray(reversed.iterator, n2, values.size)
    var i        = 0
    val diff     = Array.ofDim[Double](nwma.length)
    while (i < nwma.length) {
      diff(nwma.length - i - 1) = n2wma(i) * 2 - nwma(i)
      i = i + 1
    }
    val sqrtn = math.round(math.sqrt(n.toDouble)).toInt
    weightedAsArray(diff.iterator, sqrtn, diff.length).toList
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
