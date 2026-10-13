package currexx.backtest.optimizer

import currexx.algorithms.Parameters
import currexx.algorithms.operators.species.Distance
import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.indicator.IndicatorDistance
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.Indicator

private[optimizer] object CandidateFixtures {
  def candidate(indicator: Indicator): TestStrategy = TestStrategy(indicator, TradeStrategy(Nil, Nil))
  def any(candidates: TestStrategy*): TestStrategy  = candidate(
    Indicator.compositeAnyOf(candidates.head.indicator, candidates.tail.map(_.indicator)*)
  )
  def all(candidates: TestStrategy*): TestStrategy = candidate(
    Indicator.compositeAllOf(candidates.head.indicator, candidates.tail.map(_.indicator)*)
  )
  def space(candidate: TestStrategy, fixed: Set[Indicator] = Set.empty): Either[IllegalArgumentException, OptimisationSearchSpace] =
    OptimisationSearchSpace.forRound(
      OptimisationRound(
        "fixture",
        candidate,
        Parameters.GA(8, 1, 0.7, 0.1, 0.1, shuffle = false),
        ScoringFunction.Robust(),
        fixedIndicators = fixed,
        target = OptimisationTarget.Indicator
      )
    )
  def distance(space: OptimisationSearchSpace): Distance[TestStrategy] = new Distance[TestStrategy] {
    private val underlying = IndicatorDistance.make(space.indicatorSpace.get)
    def between(a: TestStrategy, b: TestStrategy): Either[IllegalArgumentException, Double] = underlying.between(a.indicator, b.indicator)
  }
}
