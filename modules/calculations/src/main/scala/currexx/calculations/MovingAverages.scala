package currexx.calculations

object MovingAverages {

  private val EmaSmoothing: Double = 2.0d

  /**
   * Calculates the Exponential Moving Average (EMA).
   * Returns a list in reverse chronological order (earliest to latest).
   * The list is the same size as the input.
   */
  def exponential(values: List[Double], n: Int, smoothing: Double = EmaSmoothing): List[Double] =
    if (values.isEmpty) Nil
    else {
      val k  = smoothing / (1 + n)
      val it = values.reverseIterator

      val firstPrice           = it.next()
      var prevEma              = firstPrice
      var result: List[Double] = List(firstPrice)

      while (it.hasNext) {
        val price = it.next()
        val ema   = price * k + prevEma * (1 - k)
        result = ema :: result
        prevEma = ema
      }
      result
    }

  /**
   * Calculates the Simple Moving Average (SMA).
   * For initial elements where a full window is not available, the original price is used.
   * Returns a list in reverse chronological order (earliest to latest) of the same size as the input.
   */
  def simple(values: List[Double], n: Int): List[Double] = {
    val window = collection.mutable.Queue.empty[Double]
    var result = List.empty[Double]
    val it     = values.reverseIterator

    while (it.hasNext) {
      val price = it.next()
      window.enqueue(price)
      val valueToPrepend = if (window.size < n) {
        price
      } else {
        if (window.size > n) {
          val _ = window.dequeue()
        }
        window.sum / n
      }
      result = valueToPrepend :: result
    }
    result
  }

  /**
   * Calculates the Moving Average Convergence Divergence (MACD) line.
   * Returns a list in reverse chronological order.
   */
  def macd(values: List[Double], fastLength: Int = 12, slowLength: Int = 26): List[Double] = {
    val fastMa = exponential(values, fastLength)
    val slowMa = exponential(values, slowLength)
    fastMa.zip(slowMa).map { case (fast, slow) => fast - slow }
  }

  /**
   * Calculates the MACD line and its corresponding signal line.
   * Returns two lists of the same length, in reverse chronological order.
   */
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

  /**
   * Calculates the Weighted Moving Average (WMA).
   * Returns a list in reverse chronological order of the same size as the input.
   */
  def weighted(values: List[Double], n: Int): List[Double] = {
    val window  = collection.mutable.Queue.empty[Double]
    val divider = (n * (n + 1)) / 2.0
    var result  = List.empty[Double]
    val it      = values.reverseIterator

    while (it.hasNext) {
      val price = it.next()
      window.enqueue(price)
      val valueToPrepend = if (window.size < n) {
        price
      } else {
        if (window.size > n) {
          val _ = window.dequeue()
        }
        window.zipWithIndex.foldLeft(0.0) { case (sum, (v, i)) =>
          sum + v * (i + 1)
        } / divider
      }
      result = valueToPrepend :: result
    }
    result
  }

  def hull(values: List[Double], n: Int): List[Double] = {
    val n2    = math.round(n.toDouble / 2).toInt
    val sqrtn = math.round(math.sqrt(n.toDouble)).toInt
    val wmaN  = weighted(values, n)
    val wmaN2 = weighted(values, n2)
    val diff  = wmaN2.zip(wmaN).map { case (w2, w1) => 2 * w2 - w1 }
    weighted(diff, sqrtn)
  }

  /**
   * Calculates the Triple Exponential Moving Average (TEMA).
   */
  def triple(
      values: List[Double],
      n: Int,
      maCalc: (List[Double], Int) => List[Double] = (values, n) => exponential(values, n)
  ): List[Double] = {
    val ma1 = maCalc(values, n)
    val ma2 = maCalc(ma1, n)
    val ma3 = maCalc(ma2, n)
    ma1.zip(ma2).zip(ma3).map { case ((v1, v2), v3) => (3 * v1) - (3 * v2) + v3 }
  }

  def nyquist(
      values: List[Double],
      n1: Int,
      n2: Int,
      lambda: Double,
      maCalc: (List[Double], Int) => List[Double] = weighted
  ): List[Double] = {
    val alpha = lambda * (n1 - 1) / (n1 - lambda)
    val nwma1 = maCalc(values, n1)
    val nwma2 = maCalc(nwma1, n2)
    nwma1.zip(nwma2).map { case (v1, v2) => (1 + alpha) * v1 - alpha * v2 }
  }

  /**
   * Calculates a simplified Jurik Moving Average (JMA).
   */
  def jurikSimplified(
      values: List[Double],
      length: Int,
      phase: Int,
      power: Int
  ): List[Double] =
    if (values.isEmpty) Nil
    else {
      val beta  = 0.45 * (length - 1) / (0.45 * (length - 1) + 2)
      val pr    = if (phase < -100) 0.5 else if (phase > 100) 2.5 else phase / 100.0 + 1.5
      val alpha = math.pow(beta, power)
      val it    = values.reverseIterator

      val firstPrice           = it.next()
      var ma1                  = firstPrice
      var det0                 = 0.0
      var det1                 = 0.0
      var jma                  = firstPrice
      var result: List[Double] = List(jma)

      while (it.hasNext) {
        val price   = it.next()
        val prevJma = jma
        ma1 = (1 - alpha) * price + alpha * ma1
        det0 = (price - ma1) * (1 - beta) + beta * det0
        val ma2 = ma1 + pr * det0
        det1 = (ma2 - prevJma) * math.pow(1 - alpha, 2) + math.pow(alpha, 2) * det1
        jma = prevJma + det1
        result = jma :: result
      }
      result
    }
}
