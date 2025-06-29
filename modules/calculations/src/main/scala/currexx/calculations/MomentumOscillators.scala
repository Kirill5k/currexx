package currexx.calculations

import scala.collection.mutable.Queue as MQueue

object MomentumOscillators {

  def relativeStrengthIndex(values: List[Double], length: Int): List[Double] =
    if (values.size <= length) {
      List.fill(values.size)(50.0) // Not enough data, return neutral RSI
    } else {
      val it      = values.reverseIterator
      var result  = List.empty[Double]
      var avgGain = 0.0
      var avgLoss = 0.0

      // --- Step 1: Prime the initial average gain/loss ---
      var prevValue = it.next()
      var i         = 1
      while (i < length) {
        val currentVal = it.next()
        val diff       = currentVal - prevValue
        avgGain += diff.max(0.0)
        avgLoss += diff.min(0.0).abs
        prevValue = currentVal
        // Pad results during the warm-up period
        result = 50.0 :: result
        i += 1
      }
      // Final value for the initial period
      val currentVal = it.next()
      val diff       = currentVal - prevValue
      avgGain = (avgGain + diff.max(0.0)) / length
      avgLoss = (avgLoss + diff.min(0.0).abs) / length
      prevValue = currentVal

      val rs  = if (avgLoss == 0) 100.0 else avgGain / avgLoss
      var rsi = 100.0 - (100.0 / (1.0 + rs))
      result = rsi :: result
      result = 50.0 :: result // Also pad the very first value

      // --- Step 2: Calculate the rest of the RSI using smoothing ---
      while (it.hasNext) {
        val currentVal  = it.next()
        val diff        = currentVal - prevValue
        val currentGain = diff.max(0.0)
        val currentLoss = diff.min(0.0).abs

        avgGain = (avgGain * (length - 1) + currentGain) / length
        avgLoss = (avgLoss * (length - 1) + currentLoss) / length

        val rs = if (avgLoss == 0) 100.0 else avgGain / avgLoss
        rsi = 100.0 - (100.0 / (1.0 + rs))

        result = rsi :: result
        prevValue = currentVal
      }
      result
    }

  def stochastic(
      closings: List[Double],
      highs: List[Double],
      lows: List[Double],
      length: Int
  ): List[Double] = {
    val highWindow = MQueue.empty[Double]
    val lowWindow  = MQueue.empty[Double]
    var result     = List.empty[Double]

    val data = closings.lazyZip(highs).lazyZip(lows)
    val it   = data.toList.reverseIterator
    while (it.hasNext) {
      val (close, high, low) = it.next()
      highWindow.enqueue(high)
      lowWindow.enqueue(low)

      var stoch = 0.0
      if (highWindow.size >= length) {
        // Dequeue if window is too large
        if (highWindow.size > length) {
          val _ = highWindow.dequeue()
          val _ = lowWindow.dequeue()
        }

        // The performance bug is fixed here. We call min/max on a small queue of size `length`,
        // not on a growing collection of size `i`.
        val highestHigh = highWindow.max
        val lowestLow   = lowWindow.min
        val range       = highestHigh - lowestLow

        stoch = if (range == 0) 100.0 else ((close - lowestLow) / range) * 100.0
      }

      // Prepend to build list in reverse chronological order
      result = stoch :: result
    }
    result
  }

  final private case class JrsiState(
      // Give state variables meaningful names
      f8_price: Double = 0.0,
      f28_v8_smoothed: Double = 0.0,
      f30_f28_smoothed: Double = 0.0,
      f38_vc_smoothed: Double = 0.0,
      f40_f38_smoothed: Double = 0.0,
      f48_v10_smoothed: Double = 0.0,
      f50_f48_smoothed: Double = 0.0,
      f58_abs_v8_smoothed: Double = 0.0,
      f60_f58_smoothed: Double = 0.0,
      f68_v18_smoothed: Double = 0.0,
      f70_f68_smoothed: Double = 0.0,
      f78_v1c_smoothed: Double = 0.0,
      f80_f78_smoothed: Double = 0.0,
      f88_jurik_period: Double = 0.0,
      f90_counter: Double = 0.0,
      results: List[Double] = Nil
  )

  def jurikRelativeStrengthIndex(values: List[Double], length: Int): List[Double] =
    if (values.isEmpty) Nil
    else {
      val f18 = 3.0 / (length + 2)
      val f20 = 1.0 - f18

      // Prepare chronological data and initial state
      val chronologicalValues = values.reverse
      val firstPrice          = chronologicalValues.head
      val initialState        = JrsiState(
        f8_price = 100 * firstPrice,
        // Other initial values remain 0, which is acceptable for this filter
        results = List(50.0) // Start with neutral 50
      )

      val finalState = chronologicalValues.tail.foldLeft(initialState) { (prevState, price) =>
        val f8 = 100 * price
        val v8 = f8 - prevState.f8_price

        val f28 = f20 * prevState.f28_v8_smoothed + f18 * v8
        val f30 = f18 * f28 + f20 * prevState.f30_f28_smoothed
        val vC  = 1.5 * f28 - 0.5 * f30

        val f38 = f20 * prevState.f38_vc_smoothed + f18 * vC
        val f40 = f18 * f38 + f20 * prevState.f40_f38_smoothed
        val v10 = 1.5 * f38 - 0.5 * f40

        val f48 = f20 * prevState.f48_v10_smoothed + f18 * v10
        val f50 = f18 * f48 + f20 * prevState.f50_f48_smoothed
        val v14 = 1.5 * f48 - 0.5 * f50

        val f58 = f20 * prevState.f58_abs_v8_smoothed + f18 * math.abs(v8)
        val f60 = f18 * f58 + f20 * prevState.f60_f58_smoothed
        val v18 = 1.5 * f58 - 0.5 * f60

        val f68 = f20 * prevState.f68_v18_smoothed + f18 * v18
        val f70 = f18 * f68 + f20 * prevState.f70_f68_smoothed
        val v1C = 1.5 * f68 - 0.5 * f70

        val f78 = f20 * prevState.f78_v1c_smoothed + f18 * v1C
        val f80 = f18 * f78 + f20 * prevState.f80_f78_smoothed
        val v20 = 1.5 * f78 - 0.5 * f80

        // This logic is complex and must be handled carefully, using previous state.
        val f88 = if (prevState.f90_counter == 0.0 && (length - 1) >= 5) length - 1.0 else 5.0
        val f0  = if (f88 >= prevState.f90_counter && f8 != prevState.f8_price) 1.0 else 0.0
        val f90 = if (f88 == prevState.f90_counter && f0 == 0) 0.0 else prevState.f90_counter + 1.0

        val rsx = if (f88 < f90 && v20 > 0) {
          val v4 = (v14 / v20 + 1) * 50
          v4.max(0).min(100) // Clamp between 0 and 100
        } else {
          50.0
        }

        JrsiState(
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
          f90,
          rsx :: prevState.results
        )
      }

      finalState.results
    }
}
