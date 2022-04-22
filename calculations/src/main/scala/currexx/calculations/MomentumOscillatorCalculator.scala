package currexx.calculations

object MomentumOscillatorCalculator {

  val zero = BigDecimal(0)

  def rsi(values: List[BigDecimal], length: Int): List[BigDecimal] = {
    def calc(
        remainingValues: List[BigDecimal],
        prevValue: BigDecimal,
        rsis: Array[BigDecimal],
        i: Int,
        gain: BigDecimal,
        loss: BigDecimal
    ): Array[BigDecimal] =
      if (remainingValues.isEmpty) rsis
      else {
        val diff     = remainingValues.head - prevValue
        val currGain = diff.max(zero)
        val currLoss = diff.min(zero).abs
        if (i < length) calc(remainingValues.tail, remainingValues.head, rsis, i + 1, gain + currGain, loss + currLoss)
        else if (i == length) {
          val avgGain = (gain + currLoss) / length
          val avgLoss = (loss + currLoss) / length
          val rsi = 100 - (100/(1 + avgGain / avgLoss))
          rsis(i) = rsi
          calc(remainingValues.tail, remainingValues.head, rsis, i+1, avgGain, avgLoss)
        } else {
          val avgGain = (gain * (length-1) + currGain) / length
          val avgLoss = (loss * (length-1) + currLoss) / length
          val rsi = 100 - (100/(1 + avgGain / avgLoss))
          rsis(i) = rsi
          calc(remainingValues.tail, remainingValues.head, rsis, i+1, avgGain, avgLoss)
        }
      }
    val allValues = values.reverse
    val rsis      = Array.ofDim[BigDecimal](allValues.size)
    calc(allValues.tail, allValues.head, rsis, 1, zero, zero).reverse.toList
  }
}
