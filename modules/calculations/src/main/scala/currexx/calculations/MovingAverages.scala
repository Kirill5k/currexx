package currexx.calculations

object MovingAverages {

  private val EmaSmoothing: Double = 2.0d

  /** Calculates the Exponential Moving Average (EMA).
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @param n
    *   The EMA period.
    * @return
    *   A list of EMA values, sorted from latest to earliest, same size as input.
    */
  def exponential(values: List[Double], n: Int, smoothing: Double = EmaSmoothing): List[Double] =
    if (values.isEmpty) Nil
    else {
      val k      = smoothing / (1 + n)
      val arr    = values.toArray
      val result = new Array[Double](arr.length)
      val last   = arr.length - 1

      var prevEma = arr(last)
      result(last) = prevEma
      var i = last - 1
      while (i >= 0) {
        val ema = arr(i) * k + prevEma * (1 - k)
        result(i) = ema
        prevEma = ema
        i -= 1
      }
      result.toList
    }

  /** Calculates the Simple Moving Average (SMA). For initial elements where a full window is not available, the original price is used.
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @param n
    *   The SMA period.
    * @return
    *   A list of SMA values, sorted from latest to earliest, same size as input.
    */
  def simple(values: List[Double], n: Int): List[Double] = {
    val arr        = values.toArray
    val result     = new Array[Double](arr.length)
    val window     = new Array[Double](n) // circular buffer
    var wStart     = 0
    var wSize      = 0
    var runningSum = 0.0
    var i          = arr.length - 1

    while (i >= 0) {
      val price = arr(i)
      if (wSize < n) {
        window(wSize) = price
        wSize += 1
        runningSum += price
        result(i) = if (wSize < n) price else runningSum / n
      } else {
        runningSum += price - window(wStart)
        window(wStart) = price
        wStart = (wStart + 1) % n
        result(i) = runningSum / n
      }
      i -= 1
    }
    result.toList
  }

  /** Calculates the Moving Average Convergence Divergence (MACD) line.
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   A list of MACD values, sorted from latest to earliest.
    */
  def macd(values: List[Double], fastLength: Int = 12, slowLength: Int = 26): List[Double] = {
    val fastMa = exponential(values, fastLength)
    val slowMa = exponential(values, slowLength)
    fastMa.lazyZip(slowMa).map { case (fast, slow) => fast - slow }
  }

  /** Calculates the MACD line and its corresponding signal line.
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   Two lists (MACD, Signal), both sorted from latest to earliest.
    */
  def macdWithSignal(
      values: List[Double],
      fastLength: Int = 12,
      slowLength: Int = 26,
      signalSmoothing: Int = 9
  ): (List[Double], List[Double]) = {
    val macdLine = macd(values, fastLength, slowLength)
    // `simple` takes latest->earliest and returns latest->earliest, so this works perfectly.
    val signalLine = simple(macdLine, signalSmoothing)
    (macdLine, signalLine)
  }

  /** Calculates the Weighted Moving Average (WMA).
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @param n
    *   The WMA period.
    * @return
    *   A list of WMA values, sorted from latest to earliest, same size as input.
    */
  def weighted(values: List[Double], n: Int): List[Double] = {
    val arr     = values.toArray
    val result  = new Array[Double](arr.length)
    val window  = new Array[Double](n) // circular buffer, oldest at wStart
    val divider = (n * (n + 1)) / 2.0
    var wStart  = 0
    var wSize   = 0
    var i       = arr.length - 1

    while (i >= 0) {
      val price = arr(i)
      if (wSize < n) {
        window(wSize) = price
        wSize += 1
      } else {
        window(wStart) = price
        wStart = (wStart + 1) % n
      }
      result(i) = if (wSize < n) {
        price
      } else {
        var weightedSum = 0.0
        var weight      = 1
        var j           = 0
        while (j < n) {
          weightedSum += window((wStart + j) % n) * weight
          weight += 1
          j += 1
        }
        weightedSum / divider
      }
      i -= 1
    }
    result.toList
  }

  /** Calculates the Hull Moving Average (HMA).
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   A list of HMA values, sorted from latest to earliest.
    */
  def hull(values: List[Double], n: Int): List[Double] = {
    val n2    = math.round(n.toDouble / 2).toInt
    val sqrtn = math.round(math.sqrt(n.toDouble)).toInt
    val wmaN  = weighted(values, n)
    val wmaN2 = weighted(values, n2)
    val diff  = wmaN2.lazyZip(wmaN).map { case (w2, w1) => 2 * w2 - w1 }
    // `diff` is latest->earliest, which is the correct input format for `weighted`.
    weighted(diff, sqrtn)
  }

  /** Calculates the Triple Exponential Moving Average (TEMA).
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   A list of TEMA values, sorted from latest to earliest.
    */
  def triple(
      values: List[Double],
      n: Int,
      maCalc: (List[Double], Int) => List[Double] = (values, n) => exponential(values, n)
  ): List[Double] = {
    val ma1 = maCalc(values, n)
    val ma2 = maCalc(ma1, n)
    val ma3 = maCalc(ma2, n)
    ma1.lazyZip(ma2).lazyZip(ma3).map { case (v1, v2, v3) => (3 * v1) - (3 * v2) + v3 }
  }

  /** Calculates the Nyquist Moving Average.
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   A list of Nyquist values, sorted from latest to earliest.
    */
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
    nwma1.lazyZip(nwma2).map { case (v1, v2) => (1 + alpha) * v1 - alpha * v2 }
  }

  /** Calculates a simplified Jurik Moving Average (JMA).
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @return
    *   A list of JMA values, sorted from latest to earliest.
    */
  def jurikSimplified(
      values: List[Double],
      length: Int,
      phase: Int,
      power: Int
  ): List[Double] =
    if (values.isEmpty) Nil
    else {
      val beta   = 0.45 * (length - 1) / (0.45 * (length - 1) + 2)
      val pr     = if (phase < -100) 0.5 else if (phase > 100) 2.5 else phase / 100.0 + 1.5
      val alpha  = math.pow(beta, power)
      val arr    = values.toArray
      val result = new Array[Double](arr.length)
      val last   = arr.length - 1

      var ma1  = arr(last)
      var det0 = 0.0
      var det1 = 0.0
      var jma  = arr(last)
      result(last) = jma
      var i = last - 1

      while (i >= 0) {
        val price   = arr(i)
        val prevJma = jma
        ma1  = (1 - alpha) * price + alpha * ma1
        det0 = (price - ma1) * (1 - beta) + beta * det0
        val ma2 = ma1 + pr * det0
        det1 = (ma2 - prevJma) * math.pow(1 - alpha, 2) + math.pow(alpha, 2) * det1
        jma  = prevJma + det1
        result(i) = jma
        i -= 1
      }
      result.toList
    }
}
