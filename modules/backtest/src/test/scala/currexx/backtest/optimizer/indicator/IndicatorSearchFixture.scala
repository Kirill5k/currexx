package currexx.backtest.optimizer.indicator

import cats.effect.IO
import currexx.algorithms.Population
import currexx.algorithms.operators.{Crossover, Initialiser, Mutator}
import currexx.backtest.TestStrategy
import currexx.backtest.optimizer.{
  OptimisationRound,
  OptimisationRounds,
  OptimisationSearchOperators,
  OptimisationSearchSpace,
  OptimisationTarget,
  ScoringFunction
}
import currexx.domain.signal.Indicator

import scala.util.Random

/** Run indicator-focused assertions through the production complete-candidate boundary. */
private[indicator] object IndicatorSearchFixture {
  def space(strategy: TestStrategy, fixed: Set[Indicator] = Set.empty): Either[IllegalArgumentException, OptimisationSearchSpace] =
    OptimisationSearchSpace.forRound(
      OptimisationRound(
        "indicator-test",
        strategy,
        OptimisationRounds.gaParameters,
        ScoringFunction.Robust(),
        fixedIndicators = fixed,
        target = OptimisationTarget.Indicator
      )
    )

  def operators(strategy: TestStrategy, seeds: List[Indicator] = Nil, fixed: Set[Indicator] = Set.empty)(using
      Random
  ): IO[IndicatorSearchOperators[IO]] =
    for
      search   <- IO.fromEither(space(strategy, fixed))
      complete <- OptimisationSearchOperators.make[IO](search, seeds.map(indicator => strategy.copy(indicator = indicator)))
    yield IndicatorSearchOperators(
      new Initialiser[IO, Indicator] {
        override def initialisePopulation(seed: Indicator, size: Int, shuffle: Boolean): IO[Population[Indicator]] =
          complete.initialiser.initialisePopulation(strategy.copy(indicator = seed), size, shuffle).map(_.map(_.indicator))
      },
      new Crossover[IO, Indicator] {
        override def cross(first: Indicator, second: Indicator)(using Random): IO[Indicator] =
          complete.crossover.cross(strategy.copy(indicator = first), strategy.copy(indicator = second)).map(_.indicator)
        override def cross(first: Indicator, second: Indicator, probability: Double)(using Random): IO[Indicator] =
          complete.crossover.cross(strategy.copy(indicator = first), strategy.copy(indicator = second), probability).map(_.indicator)
      },
      new Mutator[IO, Indicator] {
        override def mutate(candidate: Indicator, probability: Double)(using Random): IO[Indicator] =
          complete.mutator.mutate(strategy.copy(indicator = candidate), probability).map(_.indicator)
      }
    )
}
