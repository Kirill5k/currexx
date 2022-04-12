package currexx.calculations

object MovingAverageCalculator {

  private val EmaSmoothing: Double = 2.0D

  def sma(values: List[BigDecimal]): BigDecimal =
    values.sum / values.size

  def ema(values: List[Double], n: Int): Double =
    val k = EmaSmoothing / (1 + n)
    def calc(prices: List[Double]): Double =
      if (prices.size == 1) prices.head
      else prices.head * k + calc(prices.tail) * (1-k)
    calc(values)

}
