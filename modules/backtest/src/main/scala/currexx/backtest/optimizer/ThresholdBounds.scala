package currexx.backtest.optimizer

import currexx.domain.signal.ValueTransformation as VT

/** The band a `ThresholdCrossing`'s boundaries are searched in, which is a property of the transformation and not a constant.
  *
  * A boundary only means anything in the units of the line it is compared against, and those units differ by orders of magnitude across the
  * transformations that feed one: RSX and STOCH are percentages, WilliamsR is a negative percentage, CMF is a ratio in [-1, 1]. Searching
  * every one of them in the percentage range puts most of the space where the line can never reach, and because the operators clamp to that
  * range rather than reject outside it, a candidate that leaves is a candidate that cannot come back.
  *
  * That is not hypothetical. It is what happened to every s12 round of 2026-08-08: CMF thresholds of +/-0.17 were mutated to >= 50, could
  * not return through a clamp whose floor was 50, and produced a detector that never fires — `Condition.thresholdCrossing` has no reachable
  * branch when the line is bounded by 1 and the nearer boundary is 5 — so no signal was emitted, no trade was opened, and the fitness of
  * the whole population was zero by construction.
  *
  * Percentage oscillators retain the familiar 50..95 upper and 5..50 lower search regions with a step of 1.0, while transformations that
  * use different units receive their own reachable range.
  */
object ThresholdBounds:

  /** One transformation's output range, and where in it each boundary is allowed to sit.
    *
    * The upper boundary is confined to the top half and the lower boundary to the bottom half, both kept off their outer limit. That is the
    * prior the hardcoded percentage ranges encoded: a threshold pair is meant to mark opposite extremes of the line's travel, and a lower
    * CMF threshold above zero would label buying pressure as oversold.
    */
  final case class Band(min: Double, max: Double):
    val span: Double     = max - min
    val step: Double     = span / 100.0
    val upperMin: Double = min + span * 0.50
    val upperMax: Double = min + span * 0.95
    val lowerMin: Double = min + span * 0.05
    val lowerMax: Double = min + span * 0.50

    /** Rounds to the band's step, then to four decimals so that a step of 0.02 does not accumulate binary-fraction noise. */
    def snap(value: Double): Double = math.round(math.round(value / step) * step * 10000.0) / 10000.0

    def clampUpper(value: Double): Double = math.max(upperMin, math.min(upperMax, value))
    def clampLower(value: Double): Double = math.max(lowerMin, math.min(lowerMax, value))

  private val percentage = Band(0.0, 100.0)

  /** ADX is left on the percentage band deliberately. It is nominally 0..100 but rarely travels past 60, so its upper boundary is as
    * unreachable in practice as CMF's was in principle; nothing in the current catalogue crosses a threshold on ADX, and narrowing it would
    * change a search no strategy is currently running. Revisit it alongside the first strategy that needs one.
    */
  def of(vt: VT): Band = outputBand(None, vt).getOrElse(percentage)

  /** Tracks the output range through a sequence. Simple weighted averages preserve the range of their input; oscillators establish a new
    * range of their own. Other transformations retain the standalone fallback because their output is not bounded by their input.
    */
  private def outputBand(input: Option[Band], transformation: VT): Option[Band] =
    transformation match
      case VT.Sequenced(sequence)                           => sequence.foldLeft(input)(outputBand)
      case _: VT.SMA | _: VT.EMA | _: VT.WMA                => input.orElse(Some(percentage))
      case _: VT.RSX | _: VT.JRSX | _: VT.STOCH | _: VT.ADX => Some(percentage)
      case _: VT.WilliamsR                                  => Some(Band(-100.0, 0.0))
      case _: VT.CCI                                        => Some(Band(-250.0, 250.0))
      case _: VT.CMF                                        => Some(Band(-1.0, 1.0))
      case _                                                => Some(percentage)
