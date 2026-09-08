package currexx.backtest.optimizer

import currexx.domain.signal.ValueTransformation as VT

/** The range every searchable gene is allowed to take, in one place, because the initialiser and the mutator have to agree on it.
  *
  * They did not. Each held its own literals until 2026-09-01, and they disagreed on almost every gene: the initialiser drew moving-average
  * lengths from [2, 42] where the mutator ranged over [5, 100], so a shuffled population started in the bottom 40% of its own search space
  * and could only reach the rest by drift - the same defect the JMA bound had, present everywhere else at the same time. Draws below a
  * mutator floor were worse than narrow, they were values mutation would silently clamp the first time it touched the gene, and NMA's
  * lambda was drawn across [0.25, 20.0] against a searchable [0.5, 4.0], so four draws in five began outside the space entirely.
  *
  * A range therefore belongs to the gene rather than to the operator reading it. `IndicatorInitialiser` draws inside these, and
  * `IndicatorMutator` walks inside them; nothing else should carry a bound of its own. `ThresholdBounds` stays separate because a
  * threshold's range is a property of the transformation feeding it rather than a constant, which is a different kind of fact.
  */
object GeneBounds:

  /** Whether the operators walk a gene in equal steps or in equal proportions.
    *
    * A lookback is a proportional quantity. Five bars to ten is the same change to a moving average as fifty to a hundred, so a step of a
    * fixed number of bars means two different things at either end of [5, 100]: it is a rewrite of a fast line and a nudge to a slow one.
    * Drawn and mutated in log-space, a step means the same thing everywhere, and the bottom of the range - where most of the catalogue
    * lives - is reachable at the resolution it is actually distinguished at.
    *
    * A gene that is genuinely additive stays `Linear`, as does anything that crosses zero and so has no logarithm to take. The choice is
    * recorded here per gene rather than inferred from the sign of a floor, because inferring it means a range declared with a floor of zero
    * silently changes how it is searched.
    */
  enum Scale:
    case Linear, Logarithmic

  final case class IntRange(min: Int, max: Int, scale: Scale):
    val span: Int              = max - min
    def clamp(value: Int): Int = math.max(min, math.min(max, value))
    def isLogarithmic: Boolean = scale == Scale.Logarithmic

    /** The sub-range whose values leave room for a companion `ratio` times longer, so a related pair can be drawn without either end
      * landing on a clamp. Never narrower than a point at `min`, and always on the same scale as the range it narrows.
      */
    def leavingRoomFor(ratio: Double, companion: IntRange): IntRange =
      copy(max = math.max(min, math.min(max, (companion.max / ratio).toInt)))

  object IntRange:
    /** A gene walked in proportions. The floor has to be positive: there is no proportional step away from zero, and no logarithm of it. */
    def log(min: Int, max: Int): IntRange =
      require(min > 0, s"a logarithmic gene needs a positive floor, got [$min, $max]")
      IntRange(min, max, Scale.Logarithmic)

    /** A gene walked in equal steps, for the quantities where a step is a step wherever it starts. */
    def linear(min: Int, max: Int): IntRange = IntRange(min, max, Scale.Linear)

  final case class DoubleRange(min: Double, max: Double, step: Double):
    val span: Double = max - min

    /** Rounds to the gene's step, holds it inside the range, then to four decimals so a step of 0.005 cannot accumulate binary-fraction
      * noise. The order matters and is the one both operators have always used.
      */
    def snap(value: Double): Double =
      val rounded = math.round(value / step) * step
      val bounded = math.max(min, math.min(rounded, max))
      math.round(bounded * 10000.0) / 10000.0

    /** Into the range, and nowhere else. The step is where an operator is allowed to place a *new* value, not a property a value has to
      * have to be legal.
      */
    def clamp(value: Double): Double = math.max(min, math.min(max, value))

  // Moving averages and other smoothers, which are allowed to run slow enough to act as a regime line.
  val maLength: IntRange          = IntRange.log(5, 100)
  val standardDeviation: IntRange = IntRange.log(5, 100)
  // Oscillators, whose length is a lookback rather than a trend and stops being meaningful long before 100.
  val oscillatorLength: IntRange = IntRange.log(5, 50)
  val jmaLength: IntRange        = IntRange.log(5, 100)
  // Phase is a shift rather than a lookback: it crosses zero, and moving it by ten means much the same wherever it started.
  val jmaPhase: IntRange = IntRange.linear(-100, 100)
  // Power moves the curve more than length does and the catalogue sits at the bottom of the range, so it is proportional too.
  val jmaPower: IntRange               = IntRange.log(1, 10)
  val nmaLength: IntRange              = IntRange.log(5, 50)
  val nmaSignalLength: IntRange        = IntRange.log(5, 50)
  val nmaLambda: DoubleRange           = DoubleRange(0.5, 4.0, 0.25)
  val adxLength: IntRange              = IntRange.log(7, 50)
  val cciLength: IntRange              = IntRange.log(10, 50)
  val ichimokuLength: IntRange         = IntRange.log(9, 52)
  val cmfLength: IntRange              = IntRange.log(10, 40)
  val kalmanGain: DoubleRange          = DoubleRange(0.01, 0.5, 0.01)
  val kalmanNoise: DoubleRange         = DoubleRange(0.01, 1.0, 0.01)
  val sarAfStart: DoubleRange          = DoubleRange(0.01, 0.05, 0.005)
  val sarAfMax: DoubleRange            = DoubleRange(0.1, 0.4, 0.01)
  val sarAfStep: DoubleRange           = DoubleRange(0.01, 0.05, 0.005)
  val atrLength: IntRange              = IntRange.log(5, 50)
  val keltnerMultiplier: DoubleRange   = DoubleRange(0.5, 5.0, 0.1)
  val stdDevLength: IntRange           = IntRange.log(5, 50)
  val bollingerMultiplier: DoubleRange = DoubleRange(1.0, 4.0, 0.1)

  /** The range of a transformation's one length gene, for the callers that need to relate two of them without knowing which types they got.
    * Transformations with no single length answer with the oscillator range, which is only ever used as a fallback width.
    */
  def lengthRange(vt: VT): IntRange = vt match
    case _: VT.StandardDeviation                                  => standardDeviation
    case _: VT.SMA | _: VT.EMA | _: VT.WMA | _: VT.HMA            => maLength
    case _: VT.JMA                                                => jmaLength
    case _: VT.RSX | _: VT.JRSX | _: VT.STOCH | _: VT.ATR         => oscillatorLength
    case _: VT.WilliamsR                                          => oscillatorLength
    case _: VT.NMA                                                => nmaLength
    case _: VT.ADX                                                => adxLength
    case _: VT.CCI                                                => cciLength
    case _: VT.IchimokuKijunSen                                   => ichimokuLength
    case _: VT.CMF                                                => cmfLength
    case _: VT.Kalman | _: VT.KalmanVelocity | _: VT.ParabolicSAR => oscillatorLength
    case _: VT.Sequenced                                          => oscillatorLength

  /** The transformation's length, where it has exactly one. `None` means there is nothing for a relational draw to hold on to. */
  def lengthOf(vt: VT): Option[Int] = vt match
    case VT.StandardDeviation(l) => Some(l)
    case VT.SMA(l)               => Some(l)
    case VT.EMA(l)               => Some(l)
    case VT.WMA(l)               => Some(l)
    case VT.HMA(l)               => Some(l)
    case VT.JMA(l, _, _)         => Some(l)
    case VT.RSX(l)               => Some(l)
    case VT.JRSX(l)              => Some(l)
    case VT.STOCH(l)             => Some(l)
    case VT.ATR(l)               => Some(l)
    case VT.WilliamsR(l)         => Some(l)
    case VT.NMA(l, _, _, _)      => Some(l)
    case VT.ADX(l)               => Some(l)
    case VT.CCI(l)               => Some(l)
    case VT.IchimokuKijunSen(l)  => Some(l)
    case VT.CMF(l)               => Some(l)
    case _                       => None

  /** Replaces the transformation's length, clamped into its own range, leaving every other gene alone. */
  def withLength(vt: VT, length: Int): VT =
    val l = lengthRange(vt).clamp(length)
    vt match
      case _: VT.StandardDeviation       => VT.StandardDeviation(l)
      case _: VT.SMA                     => VT.SMA(l)
      case _: VT.EMA                     => VT.EMA(l)
      case _: VT.WMA                     => VT.WMA(l)
      case _: VT.HMA                     => VT.HMA(l)
      case VT.JMA(_, phase, power)       => VT.JMA(l, phase, power)
      case _: VT.RSX                     => VT.RSX(l)
      case _: VT.JRSX                    => VT.JRSX(l)
      case _: VT.STOCH                   => VT.STOCH(l)
      case _: VT.ATR                     => VT.ATR(l)
      case _: VT.WilliamsR               => VT.WilliamsR(l)
      case VT.NMA(_, sl, lambda, maCalc) => VT.NMA(l, sl, lambda, maCalc)
      case _: VT.ADX                     => VT.ADX(l)
      case _: VT.CCI                     => VT.CCI(l)
      case _: VT.IchimokuKijunSen        => VT.IchimokuKijunSen(l)
      case _: VT.CMF                     => VT.CMF(l)
      case other                         => other
