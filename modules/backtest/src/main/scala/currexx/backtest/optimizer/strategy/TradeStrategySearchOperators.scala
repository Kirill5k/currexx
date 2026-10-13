package currexx.backtest.optimizer.strategy

import cats.effect.Sync
import cats.syntax.flatMap.*
import currexx.algorithms.operators.{Crossover, Mutator}
import currexx.algorithms.operators.species.Distance
import currexx.backtest.optimizer.Checks
import currexx.core.trade.TradeStrategy

import scala.util.Random

/** Effectful numeric rule variation; population construction belongs to the complete-candidate search. */
object TradeStrategySearchOperators {
  def crossover[F[_]](space: TradeStrategySearchSpace)(using F: Sync[F]): Crossover[F, TradeStrategy] = new Crossover[F, TradeStrategy] {
    override def cross(first: TradeStrategy, second: TradeStrategy)(using Random): F[TradeStrategy] =
      F.delay(space.cross(first, second)).flatMap(F.fromEither)

    override def cross(first: TradeStrategy, second: TradeStrategy, crossoverProbability: Double)(using Random): F[TradeStrategy] =
      F.delay(space.cross(first, second, crossoverProbability)).flatMap(F.fromEither)
  }

  def mutator[F[_]](space: TradeStrategySearchSpace)(using F: Sync[F]): Mutator[F, TradeStrategy] = new Mutator[F, TradeStrategy] {
    override def mutate(candidate: TradeStrategy, mutationProbability: Double)(using Random): F[TradeStrategy] =
      F.fromEither(Checks.probability(mutationProbability)).flatMap(_ => mutateChecked[F](space, candidate, mutationProbability))
  }

  def distance(space: TradeStrategySearchSpace): Distance[TradeStrategy] = new Distance[TradeStrategy] {
    override def between(first: TradeStrategy, second: TradeStrategy): Either[IllegalArgumentException, Double] =
      space.distance(first, second)
  }

  private[optimizer] def mutateChecked[F[_]](space: TradeStrategySearchSpace, candidate: TradeStrategy, probability: Double)(using
      F: Sync[F],
      random: Random
  ): F[TradeStrategy] = F.delay(space.mutate(candidate, probability)).flatMap(F.fromEither)

  def randomise[F[_]](space: TradeStrategySearchSpace, candidate: TradeStrategy)(using F: Sync[F], random: Random): F[TradeStrategy] =
    F.delay(space.randomise(candidate)).flatMap(F.fromEither)

  def nearby[F[_]](space: TradeStrategySearchSpace, candidate: TradeStrategy, radius: Double)(using
      F: Sync[F],
      random: Random
  ): F[TradeStrategy] =
    F.delay(Checks.scale(radius).flatMap(_ => space.nearby(candidate, radius))).flatMap(F.fromEither)

}
