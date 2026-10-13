package currexx.backtest.optimizer.strategy

import scala.concurrent.duration.*
import scala.util.Random

/** Small value domains shared by the rule operators. These do not encode or store candidates. */
private[strategy] trait ParameterDomain[A] {
  def active: Boolean
  def contains(value: A): Boolean
  def sample(using Random): A
  def mutate(value: A, sigmaScale: Double)(using Random): A
  def mutateWithProbability(value: A, probability: Double)(using random: Random): A =
    if (!active || probability == 0 || random.nextDouble() >= probability) value else mutate(value, 1.0)
  def nearby(value: A, radius: Double)(using Random): A = mutate(value, radius)
  def cross(first: A, second: A)(using Random): A
  def distance(first: A, second: A): Double

}

private[strategy] object ParameterDomain {
  def real(range: RuleParameterBounds.DoubleRange): Either[IllegalArgumentException, ParameterDomain[Double]] =
    val validLimits = range.min.isFinite && range.max.isFinite && range.step.isFinite && range.step > 0 && range.max >= range.min &&
      (range.max - range.min).isFinite
    if (!validLimits) Left(new IllegalArgumentException("Numeric bounds need finite ordered limits and a positive finite step"))
    else
      val span  = BigDecimal.valueOf(range.max) - BigDecimal.valueOf(range.min)
      val step  = BigDecimal.valueOf(range.step)
      val steps = (span / step).toBigIntExact
      Either.cond(
        (span % step) == 0 && steps.exists(_ <= Int.MaxValue - 1),
        new Real(range),
        new IllegalArgumentException("Numeric bounds must contain a whole number of steps, with at most Int.MaxValue values")
      )

  private def nanos(value: FiniteDuration): BigInt = BigInt(value.length) * value.unit.toNanos(1)

  def duration(range: RuleParameterBounds.DurationRange): Either[IllegalArgumentException, ParameterDomain[FiniteDuration]] =
    val min  = nanos(range.min)
    val max  = nanos(range.max)
    val step = nanos(range.step)
    Either.cond(
      min >= 0 && max >= min && max <= Long.MaxValue && step > 0 && step <= Long.MaxValue &&
        (max - min) % step == 0 && (max - min) / step <= Int.MaxValue - 1,
      new DurationValues(min.toLong, max.toLong, step.toLong),
      new IllegalArgumentException(
        "Duration bounds need nonnegative ordered limits, a positive nanosecond step, and a whole number of steps"
      )
    )

  final private class Real(range: RuleParameterBounds.DoubleRange) extends ParameterDomain[Double] {
    private val min                    = BigDecimal.valueOf(range.min)
    private val max                    = BigDecimal.valueOf(range.max)
    private val step                   = BigDecimal.valueOf(range.step)
    private val steps                  = ((max - min) / step).toIntExact
    private def at(index: Int): Double = (min + step * index).min(max).toDouble

    // Search represented values: a decimal grid point's Double can be nearer to a different decimal index.
    // Use the same index for exact membership and snapping, with the upper neighbour winning midpoint ties.
    private def nearestRepresentedIndex(value: Double): Int =
      var low  = 0
      var high = steps
      while (low < high) {
        val middle = low + (high - low) / 2
        if (at(middle) < value) low = middle + 1 else high = middle
      }
      if (low == 0 || at(low) == value) low
      else
        val decimal = BigDecimal.valueOf(value)
        val below   = decimal - BigDecimal.valueOf(at(low - 1))
        val above   = BigDecimal.valueOf(at(low)) - decimal
        if (below < above) low - 1 else low

    private def snap(value: Double): Double =
      val bounded = math.max(range.min, math.min(range.max, value))
      at(nearestRepresentedIndex(bounded))
    override val active: Boolean                  = range.min != range.max
    override def contains(value: Double): Boolean =
      value.isFinite && value >= range.min && value <= range.max && at(nearestRepresentedIndex(value)) == value
    override def sample(using random: Random): Double = if (active) at(random.nextInt(steps + 1)) else range.min
    override def mutate(value: Double, sigmaScale: Double)(using random: Random): Double =
      if (!active || sigmaScale == 0) value else snap(value + random.nextGaussian() * (range.max - range.min) * 0.1 * sigmaScale)
    override def cross(first: Double, second: Double)(using random: Random): Double =
      if (first == second || !active) first
      else
        val alpha = random.nextDouble()
        math.max(math.min(first, second), math.min(math.max(first, second), snap(first * alpha + second * (1.0 - alpha))))
    override def distance(first: Double, second: Double): Double =
      if (!active) 0.0 else math.abs(first - second) / (range.max - range.min)

  }

  final private class DurationValues(min: Long, max: Long, step: Long) extends ParameterDomain[FiniteDuration] {
    private val span                                      = BigInt(max) - min
    private val steps                                     = (span / step).toInt
    private def index(value: FiniteDuration): Int         = ((nanos(value) - min) / step).toInt
    private def at(index: Int): FiniteDuration            = (BigInt(min) + BigInt(step) * index).toLong.nanos.toCoarsest
    private def snap(index: Double): FiniteDuration       = at(math.round(index).max(0).min(steps).toInt)
    override val active: Boolean                          = min != max
    override def contains(value: FiniteDuration): Boolean =
      val n = nanos(value)
      n >= min && n <= max && (n - min) % step == 0
    override def sample(using random: Random): FiniteDuration = if (active) at(random.nextInt(steps + 1)) else min.nanos.toCoarsest
    override def mutate(value: FiniteDuration, sigmaScale: Double)(using random: Random): FiniteDuration =
      if (!active || sigmaScale == 0) value
      else snap(index(value).toDouble + random.nextGaussian() * steps * 0.1 * sigmaScale)
    override def cross(first: FiniteDuration, second: FiniteDuration)(using random: Random): FiniteDuration =
      if (first == second || !active) first
      else
        val alpha = random.nextDouble()
        snap(index(first).toDouble * alpha + index(second).toDouble * (1.0 - alpha))
    override def distance(first: FiniteDuration, second: FiniteDuration): Double =
      if (!active) 0.0 else (nanos(first) - nanos(second)).abs.toDouble / span.toDouble
  }
}
