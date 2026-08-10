package currexx.backtest.optimizer

import currexx.domain.signal.ValueTransformation as VT

/** The band a `ThresholdCrossing`'s boundaries are searched in, which is a property of the transformation and not a constant.
  *
  * A boundary only means anything in the units of the line it is compared against, and those units differ by orders of magnitude across
  * the transformations that feed one: RSX and STOCH are percentages, WilliamsR is a negative percentage, CMF is a ratio in [-1, 1].
  * Searching every one of them in the percentage range puts most of the space where the line can never reach, and because the operators
  * clamp to that range rather than reject outside it, a candidate that leaves is a candidate that cannot come back.
  *
  * That is not hypothetical. It is what happened to every s12 round of 2026-08-08: CMF thresholds of +/-0.17 were mutated to >= 50, could
  * not return through a clamp whose floor was 50, and produced a detector that never fires — `Condition.thresholdCrossing` has no
  * reachable branch when the line is bounded by 1 and the nearer boundary is 5 — so no signal was emitted, no trade was opened, and the
  * fitness of the whole population was zero by construction.
  *
  * The percentage band reproduces the constants those operators used to hardcode (upper 50..95, lower 5..upper, step 1.0), so the search
  * for a percentage oscillator is unchanged and only the transformations that were never representable gain anything.
  */
object ThresholdBounds:

  /** One transformation's output range, and where in it each boundary is allowed to sit.
    *
    * The upper boundary is confined to the top half and the lower boundary kept off the floor, which is the prior the hardcoded 50..95
    * and 5.. encoded: a threshold pair is meant to mark the extremes of the line's travel, and one that sits in the middle of the range
    * fires on noise.
    */
  final case class Band(min: Double, max: Double):
    val span: Double     = max - min
    val step: Double     = span / 100.0
    val upperMin: Double = min + span * 0.50
    val upperMax: Double = min + span * 0.95
    val lowerMin: Double = min + span * 0.05

    /** Rounds to the band's step, then to four decimals so that a step of 0.02 does not accumulate binary-fraction noise. */
    def snap(value: Double): Double = math.round(math.round(value / step) * step * 10000.0) / 10000.0

  private val percentage = Band(0.0, 100.0)

  /** ADX is left on the percentage band deliberately. It is nominally 0..100 but rarely travels past 60, so its upper boundary is as
    * unreachable in practice as CMF's was in principle; nothing in the current catalogue crosses a threshold on ADX, and narrowing it
    * would change a search no strategy is currently running. Revisit it alongside the first strategy that needs one.
    */
  def of(vt: VT): Band = vt match
    case _: VT.RSX | _: VT.JRSX | _: VT.STOCH | _: VT.ADX => percentage
    case _: VT.WilliamsR                                  => Band(-100.0, 0.0)
    case _: VT.CCI                                        => Band(-250.0, 250.0)
    case _: VT.CMF                                        => Band(-1.0, 1.0)
    // A sequence is read by whatever it ends with, since that is the transformation whose output the boundary is compared against.
    case VT.Sequenced(sequence) => sequence.lastOption.fold(percentage)(of)
    case _                      => percentage
