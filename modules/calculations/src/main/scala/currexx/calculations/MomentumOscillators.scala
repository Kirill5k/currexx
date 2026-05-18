package currexx.calculations

import scala.collection.mutable.{ListBuffer, Queue as MQueue}

object MomentumOscillators {

  /** Calculates the Relative Strength Index (RSI). This version is corrected to precisely match the standard RSI calculation method,
    * ensuring the initial average is calculated correctly before applying smoothing.
    *
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @param length
    *   The RSI period.
    * @return
    *   A list of RSI values, sorted from latest to earliest, same size as input.
    */
  def relativeStrengthIndex(values: List[Double], length: Int): List[Double] =
    // RSI requires at least `length` periods of price changes, so `length + 1` prices.
    if (values.size <= length) List.fill(values.size)(50.0) // Not enough data, return neutral RSI
    else {
      val chronologicalValues = values.reverse
      val it                  = chronologicalValues.iterator
      val resultBuffer        = new ListBuffer[Double]

      // --- Step 1: Prime the initial average gain/loss using a Simple Moving Average ---
      var gainSum   = 0.0
      var lossSum   = 0.0
      var prevValue = it.next()

      // Pad the result buffer for the initial `length` periods where RSI is not yet available.
      resultBuffer ++= List.fill(length)(50.0)

      var i = 1
      while (i <= length) {
        val currentVal = it.next()
        val diff       = currentVal - prevValue
        gainSum += diff.max(0.0)
        lossSum += diff.min(0.0).abs
        prevValue = currentVal
        i += 1
      }

      var avgGain = gainSum / length
      var avgLoss = lossSum / length

      // Calculate the very first RSI value and add it to the buffer.
      val firstRs  = if (avgLoss == 0.0) 100.0 else avgGain / avgLoss
      val firstRsi = 100.0 - (100.0 / (1.0 + firstRs))
      resultBuffer += firstRsi

      // --- Step 2: Calculate the rest of the RSI using Wilder's smoothing ---
      while (it.hasNext) {
        val currentVal  = it.next()
        val diff        = currentVal - prevValue
        val currentGain = diff.max(0.0)
        val currentLoss = diff.min(0.0).abs

        // Apply Wilder's smoothing using the previous average
        avgGain = (avgGain * (length - 1) + currentGain) / length
        avgLoss = (avgLoss * (length - 1) + currentLoss) / length

        val rs  = if (avgLoss == 0.0) 100.0 else avgGain / avgLoss
        val rsi = 100.0 - (100.0 / (1.0 + rs))

        resultBuffer += rsi
        prevValue = currentVal
      }

      // The buffer is currently oldest-to-latest: [pad, pad, ..., rsi1, rsi2, ...].
      // The total size is greater than the input size because of the padding.
      // We need to take the last `values.size` elements to get the correctly aligned output.
      resultBuffer.toList.takeRight(values.size).reverse
    }

  /** Calculates the Stochastic Oscillator (%K).
    *
    * @param closings
    *   List of closing prices, sorted from latest to earliest.
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param length
    *   The Stochastic period.
    * @return
    *   A list of %K values, sorted from latest to earliest, same size as input.
    */
  def stochastic(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    // Combine inputs into a chronological stream of tuples for easier processing.
    val data = closings.lazyZip(highs).lazyZip(lows).toList

    val highWindow   = MQueue.empty[Double]
    val lowWindow    = MQueue.empty[Double]
    val resultBuffer = new ListBuffer[Double]

    val it = data.reverseIterator
    while (it.hasNext) {
      val (close, high, low) = it.next()
      highWindow.enqueue(high)
      lowWindow.enqueue(low)

      if (highWindow.size > length) {
        val _ = highWindow.dequeue()
        val _ = lowWindow.dequeue()
      }

      var stoch = 0.0 // Default to 0 during warm-up
      if (highWindow.size == length) {
        val highestHigh = highWindow.max
        val lowestLow   = lowWindow.min
        val range       = highestHigh - lowestLow
        stoch = if (range == 0.0) 100.0 else ((close - lowestLow) / range) * 100.0
      }
      resultBuffer += stoch
    }
    resultBuffer.toList.reverse
  }

  /** Calculates the Average Directional Index (ADX).
    *
    * @param closings
    *   List of closing prices, sorted from latest to earliest.
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param length
    *   The ADX smoothing period (typically 14).
    * @return
    *   A list of ADX values (0-100), sorted from latest to earliest, same size as input.
    */
  def averageDirectionalIndex(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] =
    if (closings.size < 2) List.fill(closings.size)(0.0)
    else {
      val data         = closings.lazyZip(highs).lazyZip(lows).toList.reverse // chronological
      val resultBuffer = new ListBuffer[Double]

      var prevHigh  = data.head._2
      var prevLow   = data.head._3
      var prevClose = data.head._1

      var smoothedPlusDM  = 0.0
      var smoothedMinusDM = 0.0
      var smoothedTR      = 0.0
      var smoothedADX     = 0.0
      var count           = 0

      resultBuffer += 0.0 // first element has no DM

      val it = data.iterator
      val _ = it.next() // skip first
      while (it.hasNext) {
        val (close, high, low) = it.next()
        count += 1

        val tr      = math.max(high - low, math.max(math.abs(high - prevClose), math.abs(low - prevClose)))
        val plusDM  = if (high - prevHigh > prevLow - low && high - prevHigh > 0) high - prevHigh else 0.0
        val minusDM = if (prevLow - low > high - prevHigh && prevLow - low > 0) prevLow - low else 0.0

        if (count <= length) {
          smoothedPlusDM += plusDM
          smoothedMinusDM += minusDM
          smoothedTR += tr
          if (count == length) {
            smoothedPlusDM /= length
            smoothedMinusDM /= length
            smoothedTR /= length
          }
          resultBuffer += 0.0
        } else {
          smoothedPlusDM = (smoothedPlusDM * (length - 1) + plusDM) / length
          smoothedMinusDM = (smoothedMinusDM * (length - 1) + minusDM) / length
          smoothedTR = (smoothedTR * (length - 1) + tr) / length

          val plusDI  = if (smoothedTR == 0.0) 0.0 else 100.0 * smoothedPlusDM / smoothedTR
          val minusDI = if (smoothedTR == 0.0) 0.0 else 100.0 * smoothedMinusDM / smoothedTR
          val diSum   = plusDI + minusDI
          val dx      = if (diSum == 0.0) 0.0 else 100.0 * math.abs(plusDI - minusDI) / diSum

          if (count == length + 1) {
            smoothedADX = dx
          } else {
            smoothedADX = (smoothedADX * (length - 1) + dx) / length
          }
          resultBuffer += smoothedADX
        }

        prevHigh = high
        prevLow = low
        prevClose = close
      }
      resultBuffer.toList.reverse
    }

  /** Calculates Williams %R.
    *
    * @param closings
    *   List of closing prices, sorted from latest to earliest.
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param length
    *   The lookback period (typically 14).
    * @return
    *   A list of Williams %R values (-100 to 0), sorted from latest to earliest, same size as input.
    */
  def williamsR(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val data         = closings.lazyZip(highs).lazyZip(lows).toList
    val highWindow   = MQueue.empty[Double]
    val lowWindow    = MQueue.empty[Double]
    val resultBuffer = new ListBuffer[Double]

    val it = data.reverseIterator
    while (it.hasNext) {
      val (close, high, low) = it.next()
      highWindow.enqueue(high)
      lowWindow.enqueue(low)
      if (highWindow.size > length) {
        val _ = highWindow.dequeue()
        val _ = lowWindow.dequeue()
      }

      var wr = -50.0
      if (highWindow.size == length) {
        val hh    = highWindow.max
        val ll    = lowWindow.min
        val range = hh - ll
        wr = if (range == 0.0) -50.0 else ((hh - close) / range) * -100.0
      }
      resultBuffer += wr
    }
    resultBuffer.toList.reverse
  }

  /** Calculates the Commodity Channel Index (CCI).
    *
    * @param closings
    *   List of closing prices, sorted from latest to earliest.
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param length
    *   The CCI period (typically 20).
    * @return
    *   A list of CCI values (unbounded), sorted from latest to earliest, same size as input.
    */
  def commodityChannelIndex(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val typicalPrices = closings.lazyZip(highs).lazyZip(lows).map((c, h, l) => (h + l + c) / 3.0).toList
    val window        = MQueue.empty[Double]
    val resultBuffer  = new ListBuffer[Double]

    val it = typicalPrices.reverseIterator
    while (it.hasNext) {
      val tp = it.next()
      window.enqueue(tp)
      if (window.size > length) {
        val _ = window.dequeue()
      }

      var cci = 0.0
      if (window.size == length) {
        val mean = window.sum / length
        var madSum = 0.0
        window.foreach(v => madSum += math.abs(v - mean))
        val meanDeviation = madSum / length
        cci = if (meanDeviation == 0.0) 0.0 else (tp - mean) / (0.015 * meanDeviation)
      }
      resultBuffer += cci
    }
    resultBuffer.toList.reverse
  }

  /** Calculates the Ichimoku Kijun-Sen (Base Line). Midpoint of highest high and lowest low over N periods.
    *
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param length
    *   The lookback period (typically 26).
    * @return
    *   A list of Kijun-Sen values, sorted from latest to earliest, same size as input.
    */
  def ichimokuKijunSen(
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val highWindow   = MQueue.empty[Double]
    val lowWindow    = MQueue.empty[Double]
    val resultBuffer = new ListBuffer[Double]

    val it = highs.lazyZip(lows).toList.reverseIterator
    while (it.hasNext) {
      val (high, low) = it.next()
      highWindow.enqueue(high)
      lowWindow.enqueue(low)
      if (highWindow.size > length) {
        val _ = highWindow.dequeue()
        val _ = lowWindow.dequeue()
      }

      val kijun = if (highWindow.size == length) {
        (highWindow.max + lowWindow.min) / 2.0
      } else {
        (high + low) / 2.0
      }
      resultBuffer += kijun
    }
    resultBuffer.toList.reverse
  }

  /** Calculates the Parabolic SAR.
    *
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param afStart
    *   Initial acceleration factor (typically 0.02).
    * @param afMax
    *   Maximum acceleration factor (typically 0.2).
    * @param afStep
    *   Acceleration factor increment (typically 0.02).
    * @return
    *   A list of SAR values, sorted from latest to earliest, same size as input.
    */
  def parabolicSAR(
      highs: List[Double],
      lows: List[Double],
      afStart: Double,
      afMax: Double,
      afStep: Double
  ): List[Double] =
    if (highs.size < 2) List.fill(highs.size)(0.0)
    else {
      val hArr = highs.reverse.toArray
      val lArr = lows.reverse.toArray
      val n    = hArr.length
      val result = new Array[Double](n)

      var isLong = hArr(1) > hArr(0) || lArr(1) > lArr(0)
      var af     = afStart
      var ep     = if (isLong) hArr(0) else lArr(0)
      var sar    = if (isLong) lArr(0) else hArr(0)
      result(0) = sar

      var i = 1
      while (i < n) {
        val prevSar = sar
        sar = prevSar + af * (ep - prevSar)

        if (isLong) {
          sar = math.min(sar, lArr(i - 1))
          if (i >= 2) sar = math.min(sar, lArr(i - 2))

          if (lArr(i) < sar) {
            isLong = false
            sar = ep
            ep = lArr(i)
            af = afStart
          } else {
            if (hArr(i) > ep) {
              ep = hArr(i)
              af = math.min(af + afStep, afMax)
            }
          }
        } else {
          sar = math.max(sar, hArr(i - 1))
          if (i >= 2) sar = math.max(sar, hArr(i - 2))

          if (hArr(i) > sar) {
            isLong = true
            sar = ep
            ep = hArr(i)
            af = afStart
          } else {
            if (lArr(i) < ep) {
              ep = lArr(i)
              af = math.min(af + afStep, afMax)
            }
          }
        }
        result(i) = sar
        i += 1
      }
      result.toList.reverse
    }

  /** Calculates the Chaikin Money Flow (CMF).
    *
    * @param closings
    *   List of closing prices, sorted from latest to earliest.
    * @param highs
    *   List of high prices, sorted from latest to earliest.
    * @param lows
    *   List of low prices, sorted from latest to earliest.
    * @param volumes
    *   List of volume values, sorted from latest to earliest.
    * @param length
    *   The CMF period (typically 20).
    * @return
    *   A list of CMF values (-1 to 1), sorted from latest to earliest, same size as input.
    */
  def chaikinMoneyFlow(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      volumes: List[Double],
      length: Int
  ): List[Double] = {
    val mfvWindow    = MQueue.empty[Double]
    val volWindow    = MQueue.empty[Double]
    val resultBuffer = new ListBuffer[Double]

    val data = closings.lazyZip(highs).lazyZip(lows).toList.lazyZip(volumes).toList
    val it   = data.reverseIterator
    while (it.hasNext) {
      val ((close, high, low), volume) = it.next()
      val range = high - low
      val mfMultiplier = if (range == 0.0) 0.0 else ((close - low) - (high - close)) / range
      val mfVolume     = mfMultiplier * volume

      mfvWindow.enqueue(mfVolume)
      volWindow.enqueue(volume)
      if (mfvWindow.size > length) {
        val _ = mfvWindow.dequeue()
        val _ = volWindow.dequeue()
      }

      var cmf = 0.0
      if (mfvWindow.size == length) {
        val volSum = volWindow.sum
        cmf = if (volSum == 0.0) 0.0 else mfvWindow.sum / volSum
      }
      resultBuffer += cmf
    }
    resultBuffer.toList.reverse
  }

  // A mutable class is better for a while loop to avoid creating many case class instances.
  final private class JrsiState {
    var f8_price: Double            = 0.0
    var f28_v8_smoothed: Double     = 0.0
    var f30_f28_smoothed: Double    = 0.0
    var f38_vc_smoothed: Double     = 0.0
    var f40_f38_smoothed: Double    = 0.0
    var f48_v10_smoothed: Double    = 0.0
    var f50_f48_smoothed: Double    = 0.0
    var f58_abs_v8_smoothed: Double = 0.0
    var f60_f58_smoothed: Double    = 0.0
    var f68_v18_smoothed: Double    = 0.0
    var f70_f68_smoothed: Double    = 0.0
    var f78_v1c_smoothed: Double    = 0.0
    var f80_f78_smoothed: Double    = 0.0
    var f88_jurik_period: Double    = 0.0
    var f90_counter: Double         = 0.0
  }

  /** Calculates the Jurik Relative Strength Index (RSX).
    *
    * @param values
    *   A list of prices sorted from latest to earliest.
    * @param length
    *   The RSX period.
    * @return
    *   A list of RSX values, sorted from latest to earliest, same size as input.
    */
  def jurikRelativeStrengthIndex(values: List[Double], length: Int): List[Double] =
    if (values.isEmpty) Nil
    else {
      val f18 = 3.0 / (length + 2)
      val f20 = 1.0 - f18

      val chronologicalValues = values.reverse
      val it                  = chronologicalValues.iterator
      val resultBuffer        = new ListBuffer[Double]
      val state               = new JrsiState()

      // Seed the initial state
      val firstPrice = it.next()
      state.f8_price = 100 * firstPrice
      resultBuffer += 50.0 // Start with neutral 50

      while (it.hasNext) {
        val price    = it.next()
        val prev_f8  = state.f8_price
        val prev_f90 = state.f90_counter

        val f8 = 100 * price
        val v8 = f8 - prev_f8

        val f28 = f20 * state.f28_v8_smoothed + f18 * v8
        val f30 = f18 * f28 + f20 * state.f30_f28_smoothed
        val vC  = 1.5 * f28 - 0.5 * f30

        val f38 = f20 * state.f38_vc_smoothed + f18 * vC
        val f40 = f18 * f38 + f20 * state.f40_f38_smoothed
        val v10 = 1.5 * f38 - 0.5 * f40

        val f48 = f20 * state.f48_v10_smoothed + f18 * v10
        val f50 = f18 * f48 + f20 * state.f50_f48_smoothed
        val v14 = 1.5 * f48 - 0.5 * f50

        val f58 = f20 * state.f58_abs_v8_smoothed + f18 * math.abs(v8)
        val f60 = f18 * f58 + f20 * state.f60_f58_smoothed
        val v18 = 1.5 * f58 - 0.5 * f60

        val f68 = f20 * state.f68_v18_smoothed + f18 * v18
        val f70 = f18 * f68 + f20 * state.f70_f68_smoothed
        val v1C = 1.5 * f68 - 0.5 * f70

        val f78 = f20 * state.f78_v1c_smoothed + f18 * v1C
        val f80 = f18 * f78 + f20 * state.f80_f78_smoothed
        val v20 = 1.5 * f78 - 0.5 * f80

        val f88 = if (prev_f90 == 0.0 && (length - 1) >= 5) length - 1.0 else 5.0
        val f0  = if (f88 >= prev_f90 && f8 != prev_f8) 1.0 else 0.0
        val f90 = if (f88 == prev_f90 && f0 == 0.0) 0.0 else prev_f90 + 1.0

        val rsx = if (f88 < f90 && v20 > 0.0) {
          val v4 = (v14 / v20 + 1) * 50
          v4.max(0).min(100)
        } else {
          50.0
        }

        resultBuffer += rsx

        // Update state for the next iteration
        state.f8_price = f8; state.f28_v8_smoothed = f28; state.f30_f28_smoothed = f30
        state.f38_vc_smoothed = f38; state.f40_f38_smoothed = f40; state.f48_v10_smoothed = f48
        state.f50_f48_smoothed = f50; state.f58_abs_v8_smoothed = f58; state.f60_f58_smoothed = f60
        state.f68_v18_smoothed = f68; state.f70_f68_smoothed = f70; state.f78_v1c_smoothed = f78
        state.f80_f78_smoothed = f80; state.f88_jurik_period = f88; state.f90_counter = f90
      }
      resultBuffer.toList.reverse
    }
}
