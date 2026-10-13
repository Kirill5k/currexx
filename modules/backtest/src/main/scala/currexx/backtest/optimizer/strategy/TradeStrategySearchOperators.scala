package currexx.backtest.optimizer.strategy

import cats.effect.Sync
import cats.syntax.flatMap.*
import currexx.algorithms.operators.species.Distance
import currexx.backtest.optimizer.Checks
import currexx.core.trade.TradeStrategy

import scala.util.Random

/** Internal rule variation; population construction and probability validation belong to the complete-candidate search. */
private[optimizer] object TradeStrategySearchOperators {
  def cross[F[_]](space: TradeStrategySearchSpace, first: TradeStrategy, second: TradeStrategy)(using
      F: Sync[F],
      random: Random
  ): F[TradeStrategy] = F.delay(space.cross(first, second)).flatMap(F.fromEither)

  def distance(space: TradeStrategySearchSpace): Distance[TradeStrategy] = new Distance[TradeStrategy] {
    override def between(first: TradeStrategy, second: TradeStrategy): Either[IllegalArgumentException, Double] =
      space.distance(first, second)
  }

  def mutateChecked[F[_]](space: TradeStrategySearchSpace, candidate: TradeStrategy, probability: Double)(using
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
