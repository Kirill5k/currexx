package currexx.backtest.services

import cats.Monad
import cats.effect.{Concurrent, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.market.db.MarketStateRepository
import currexx.core.market.{MarketProfile, MarketService, MarketState, PositionState}
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId

final private class TestMarketStateRepository[F[_]: Monad](
    private val state: Ref[F, MarketState]
) extends MarketStateRepository[F]:
  override def delete(uid: UserId, cp: CurrencyPair): F[Unit]                                  = Monad[F].unit
  override def deleteAll(uid: UserId): F[Unit]                                                 = Monad[F].unit
  override def update(uid: UserId, pair: CurrencyPair, profile: MarketProfile): F[MarketState] =
    state.updateAndGet(_.copy(profile = profile))
  override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState] =
    state.updateAndGet(_.copy(currentPosition = position))
  override def getAll(uid: UserId): F[List[MarketState]] =
    state.get.map(List(_))

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    state.get.map(Some(_))

object TestMarketService:
  def make[F[_]: Concurrent](initialState: MarketState, dispatcher: ActionDispatcher[F]): F[MarketService[F]] =
    Ref.of(initialState).flatMap(s => MarketService.make(TestMarketStateRepository(s), dispatcher))
