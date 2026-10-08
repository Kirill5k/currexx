package currexx.backtest.walkforward

/** Each period restarts the account, so compare paired net differences without combining their equity curves or risk ratios. */
final case class WalkForwardSummary(
    completedWindows: Int,
    totalNetDifference: BigDecimal,
    medianNetDifference: Option[BigDecimal],
    worstNetDifference: Option[BigDecimal],
    positiveWindows: Int,
    tiedWindows: Int,
    negativeWindows: Int,
    candidateSelectedWindows: Int,
    baseSelectedWindows: Int,
    noCandidatePassedWindows: Int
):
  def baseRetainedWindows: Int = baseSelectedWindows + noCandidatePassedWindows

object WalkForwardSummary:
  def from(results: List[WindowResult]): WalkForwardSummary =
    val differences = results.map(_.forward.netDifference).sorted
    val median      = Option.when(differences.nonEmpty) {
      val middle = differences.size / 2
      if (differences.size % 2 == 0) (differences(middle - 1) + differences(middle)) / 2 else differences(middle)
    }
    WalkForwardSummary(
      completedWindows = results.size,
      totalNetDifference = differences.sum,
      medianNetDifference = median,
      worstNetDifference = differences.headOption,
      positiveWindows = differences.count(_ > 0),
      tiedWindows = differences.count(_ == 0),
      negativeWindows = differences.count(_ < 0),
      candidateSelectedWindows = results.count(_.selection.outcome == SelectionOutcome.CandidateSelected),
      baseSelectedWindows = results.count(_.selection.outcome == SelectionOutcome.BaseSelected),
      noCandidatePassedWindows = results.count(_.selection.outcome == SelectionOutcome.NoCandidatePassed)
    )
