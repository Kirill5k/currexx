package currexx.backtest.walkforward

import currexx.backtest.MarketDataProvider.Dataset
import currexx.backtest.OptimisationRound

import java.time.{Instant, ZoneOffset}
import java.time.format.DateTimeFormatter

final case class WalkForwardExperiment(
    id: String,
    round: OptimisationRound,
    masterSeed: Long,
    history: List[Dataset],
    plan: WalkForwardPlan
)

object WalkForwardExperiment:
  private val timestamp = DateTimeFormatter.ofPattern("yyyyMMdd-HHmmss-SSS").withZone(ZoneOffset.UTC)

  /** Generates an ID when called; effectful callers suspend creation in their existing effect. */
  def create(
      round: OptimisationRound,
      masterSeed: Long,
      history: List[Dataset],
      plan: WalkForwardPlan
  ): WalkForwardExperiment =
    val objective = round.searchObjective.productPrefix.toLowerCase
    val id        = s"${timestamp.format(Instant.now())}-${round.name}-$objective-seed-$masterSeed"
    WalkForwardExperiment(id, round, masterSeed, history, plan)
