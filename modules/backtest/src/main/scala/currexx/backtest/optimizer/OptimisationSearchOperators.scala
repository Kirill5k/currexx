package currexx.backtest.optimizer

import cats.effect.Sync
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.Population
import currexx.algorithms.operators.{Crossover, Initialiser, Mutator}
import currexx.algorithms.operators.species.Distance
import currexx.backtest.TestStrategy
import currexx.domain.signal.Indicator
import currexx.backtest.optimizer.OptimisationSearchSpace.Components
import currexx.backtest.optimizer.indicator.{IndicatorDistance, IndicatorSearchOperators, IndicatorSearchSpace}
import currexx.backtest.optimizer.strategy.{TradeStrategySearchOperators, TradeStrategySearchSpace}

import scala.util.Random

final case class OptimisationSearchOperators[F[_]](
    initialiser: Initialiser[F, TestStrategy],
    crossover: Crossover[F, TestStrategy],
    mutator: Mutator[F, TestStrategy],
    distance: Distance[TestStrategy]
)

/** Compose variation at the candidate boundary; evaluation, selection and budgets still see one population. */
object OptimisationSearchOperators {
  def make[F[_]](space: OptimisationSearchSpace, extraSeeds: List[TestStrategy] = Nil)(using
      F: Sync[F],
      random: Random
  ): F[OptimisationSearchOperators[F]] =
    F.fromEither(space.projectSeeds(extraSeeds)).flatMap { seeds =>
      space.components match
        case Components.IndicatorOnly(indicators) => indicatorOnly[F](space, indicators, seeds)
        case Components.RulesOnly(rules)          => F.pure(compose[F](space, None, rules, seeds))
        case Components.Joint(indicators, rules)  =>
          IndicatorSearchOperators.variation[F](indicators).map(operators => compose[F](space, Some(indicators -> operators), rules, seeds))
    }

  private def indicatorOnly[F[_]](
      space: OptimisationSearchSpace,
      indicators: IndicatorSearchSpace,
      seeds: List[TestStrategy]
  )(using F: Sync[F], random: Random): F[OptimisationSearchOperators[F]] =
    for
      projected <- F.fromEither(seeds.traverse(seed => indicators.project(seed.indicator)).map(_.flatten))
      operators <- IndicatorSearchOperators.fromProjectedSeeds[F](indicators, projected)
    yield {
      def restore(indicator: Indicator): TestStrategy = TestStrategy(indicator, space.template.rules)
      val distance                                    = IndicatorDistance.make(indicators)
      OptimisationSearchOperators(
        new Initialiser[F, TestStrategy] {
          override def initialisePopulation(seed: TestStrategy, size: Int, shuffle: Boolean): F[Population[TestStrategy]] =
            operators.initialiser.initialisePopulation(seed.indicator, size, shuffle).map(_.map(restore))
        },
        new Crossover[F, TestStrategy] {
          override def cross(first: TestStrategy, second: TestStrategy)(using Random): F[TestStrategy] =
            operators.crossover.cross(first.indicator, second.indicator).map(restore)
          override def cross(first: TestStrategy, second: TestStrategy, probability: Double)(using Random): F[TestStrategy] =
            F.fromEither(Checks.probability(probability))
              .flatMap(_ => operators.crossover.cross(first.indicator, second.indicator, probability))
              .map(restore)
        },
        new Mutator[F, TestStrategy] {
          override def mutate(candidate: TestStrategy, probability: Double)(using Random): F[TestStrategy] =
            F.fromEither(Checks.probability(probability))
              .flatMap(_ => operators.mutator.mutate(candidate.indicator, probability))
              .map(restore)
        },
        new Distance[TestStrategy] {
          override def between(first: TestStrategy, second: TestStrategy): Either[IllegalArgumentException, Double] =
            distance.between(first.indicator, second.indicator)
        }
      )
    }

  private def compose[F[_]](
      space: OptimisationSearchSpace,
      indicators: Option[(IndicatorSearchSpace, IndicatorSearchOperators.Variation[F])],
      rules: TradeStrategySearchSpace,
      seeds: List[TestStrategy]
  )(using F: Sync[F], random: Random): OptimisationSearchOperators[F] = {
    val ruleCrossover = TradeStrategySearchOperators.crossover[F](rules)

    def changeIndicator(operation: (IndicatorSearchSpace, IndicatorSearchOperators.Variation[F]) => F[Indicator]): F[Indicator] =
      indicators.fold(F.pure(space.template.indicator))((search, operators) => operation(search, operators))

    def jitter(candidate: TestStrategy, radius: Double): F[TestStrategy] =
      for
        indicator <- changeIndicator((search, _) => IndicatorSearchOperators.nearby[F](search, candidate.indicator, radius))
        strategy  <- TradeStrategySearchOperators.nearby[F](rules, candidate.rules, radius)
      yield TestStrategy(indicator, strategy)

    def randomise(candidate: TestStrategy): F[TestStrategy] =
      for
        indicator <- changeIndicator((search, _) => IndicatorSearchOperators.randomise[F](search, candidate.indicator))
        strategy  <- TradeStrategySearchOperators.randomise[F](rules, candidate.rules)
      yield TestStrategy(indicator, strategy)

    def parents(first: TestStrategy, second: TestStrategy): F[(TestStrategy, TestStrategy)] =
      F.fromEither((space.canonicalise(first), space.canonicalise(second)).tupled)

    def crossCandidate(first: TestStrategy, second: TestStrategy)(using Random): F[TestStrategy] =
      for
        indicator <- changeIndicator((_, operators) => operators.crossover.cross(first.indicator, second.indicator))
        strategy  <- ruleCrossover.cross(first.rules, second.rules)
      yield TestStrategy(indicator, strategy)

    val indicatorDistance = indicators.map((search, _) => IndicatorDistance.make(search))
    val strategyDistance  = TradeStrategySearchOperators.distance(rules)
    OptimisationSearchOperators(
      new Initialiser[F, TestStrategy] {
        override def initialisePopulation(seed: TestStrategy, size: Int, shuffle: Boolean): F[Population[TestStrategy]] =
          F.fromEither(space.canonicalise(seed)).flatMap { canonical =>
            SeededPopulation.build((canonical +: seeds).toVector, size, shuffle)(jitter, () => randomise(canonical))
          }
      },
      new Crossover[F, TestStrategy] {
        override def cross(first: TestStrategy, second: TestStrategy)(using Random): F[TestStrategy] =
          parents(first, second).flatMap { case (left, right) => crossCandidate(left, right) }
        override def cross(first: TestStrategy, second: TestStrategy, probability: Double)(using r: Random): F[TestStrategy] =
          F.fromEither(Checks.probability(probability)).flatMap(_ => parents(first, second)).flatMap { case (left, right) =>
            F.delay(r.nextDouble() < probability).flatMap(selected => if (selected) crossCandidate(left, right) else F.pure(left))
          }
      },
      new Mutator[F, TestStrategy] {
        override def mutate(candidate: TestStrategy, probability: Double)(using Random): F[TestStrategy] =
          F.fromEither(Checks.probability(probability).flatMap(_ => space.canonicalise(candidate))).flatMap { canonical =>
            for
              indicator <- changeIndicator((_, operators) => operators.mutator.mutate(canonical.indicator, probability))
              strategy  <- TradeStrategySearchOperators.mutateChecked[F](rules, canonical.rules, probability)
            yield TestStrategy(indicator, strategy)
          }
      },
      new Distance[TestStrategy] {
        override def between(first: TestStrategy, second: TestStrategy): Either[IllegalArgumentException, Double] =
          for
            indicator <- indicatorDistance.fold[Either[IllegalArgumentException, Double]](Right(0.0))(
              _.between(first.indicator, second.indicator)
            )
            strategy <- strategyDistance.between(first.rules, second.rules)
            distance =
              if (space.parameterCount == 0) 0.0
              else
                math.hypot(
                  indicator * math.sqrt(space.indicatorParameterCount.toDouble),
                  strategy * math.sqrt(space.strategyParameterCount.toDouble)
                ) /
                  math.sqrt(space.parameterCount.toDouble)
            finite <- Either.cond(distance.isFinite, distance, new IllegalArgumentException("Candidate distance must be finite"))
          yield finite
      }
    )
  }
}
