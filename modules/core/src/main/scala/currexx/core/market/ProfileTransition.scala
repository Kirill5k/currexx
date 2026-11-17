package currexx.core.market

import currexx.core.signal.SignalBatch
import currexx.domain.signal.{Indicator, IndicatorId, ValueRole}

/** Pure transition applied against the latest optimistic-locking snapshot. */
object ProfileTransition:
  enum Outcome:
    case Changed, Unchanged, Skipped

  final case class Result(state: MarketState, outcome: Outcome)

  def apply(state: MarketState, batch: SignalBatch): Result =
    applyCandle(reconcile(resetInterval(state, batch), batch), batch)

  private def resetInterval(state: MarketState, batch: SignalBatch): MarketState =
    if state.interval.forall(_ == batch.interval) then state
    else
      state.copy(
        profile = MarketProfile(),
        previousProfile = None,
        invalidated = state.invalidated ++ readingIds(state),
        lastCandleTime = None,
        lastTimeStateCandle = None
      )

  private def readingIds(state: MarketState): Set[IndicatorId] =
    state.profile.readings.keySet ++ state.previousProfile.toList.flatMap(_.readings.keySet)

  private def reconcile(state: MarketState, batch: SignalBatch): MarketState =
    def canAdopt(id: IndicatorId): Boolean =
      val unique       = batch.sources.values.count(i => IndicatorId.defaultFor(i).contains(id)) == 1
      val momentumSafe = id != IndicatorId.forValue(ValueRole.Momentum) || !batch.sources.values.exists(
        _.definition.isInstanceOf[Indicator.ThresholdCrossing]
      )
      state.configRevision.isEmpty && unique && momentumSafe

    val unchanged                      = state.sources == batch.sources && state.configRevision.nonEmpty
    def keep(id: IndicatorId): Boolean = batch.sources.get(id).exists { definition =>
      state.sources.get(id).contains(definition) || (state.sources.isEmpty && canAdopt(id))
    }
    val dropped                                         = if unchanged then Set.empty[IndicatorId] else readingIds(state).filterNot(keep)
    def retained(profile: MarketProfile): MarketProfile =
      if dropped.isEmpty then profile else profile.copy(readings = profile.readings -- dropped)

    state.copy(
      profile = retained(state.profile),
      previousProfile = state.previousProfile.map(retained),
      invalidated = (state.invalidated ++ dropped).intersect(batch.sources.keySet),
      configRevision = Some(batch.revision),
      interval = Some(batch.interval),
      sources = batch.sources
    )

  private def applyCandle(state: MarketState, batch: SignalBatch): Result =
    val unconsumed = state.lastCandleTime.forall(_.isBefore(batch.candleTime))
    val newTime    = unconsumed && state.lastTimeStateCandle.forall(_.isBefore(batch.candleTime))
    val newCandle  = unconsumed && state.lastTimeStateCandle.forall(!_.isAfter(batch.candleTime))
    val adjusted   = if newTime then batch.closureGap.fold(state)(state.adjustForMarketClosure(_, batch.candleTime))
    else state
    val updated = if newCandle then batch.signals.foldLeft(adjusted.profile)(MarketProfileUpdater.update) else adjusted.profile
    val changed = updated != adjusted.profile
    val next    = adjusted.copy(
      profile = updated,
      previousProfile = if changed then Some(adjusted.profile) else adjusted.previousProfile,
      lastCandleTime = if newCandle && batch.signals.nonEmpty then Some(batch.candleTime) else adjusted.lastCandleTime,
      lastTimeStateCandle = if newTime then Some(batch.candleTime) else adjusted.lastTimeStateCandle
    )
    val ready = if next.invalidated.isEmpty then Set.empty[IndicatorId]
    else next.profile.readings.keySet.intersect(next.previousProfile.toList.flatMap(_.readings.keySet).toSet)
    val outcome =
      if !newCandle then Outcome.Skipped
      else if changed then Outcome.Changed
      else Outcome.Unchanged
    Result(next.copy(invalidated = next.invalidated -- ready), outcome)
