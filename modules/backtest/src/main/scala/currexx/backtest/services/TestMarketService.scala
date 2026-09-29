package currexx.backtest.services

import cats.Monad
import cats.effect.{Concurrent, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.logging.Logger
import currexx.core.market.db.MarketStateRepository
import currexx.core.market.{MarketService, MarketState, PositionState}
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock

final private class TestMarketStateRepository[F[_]: Monad](
    private val state: Ref[F, MarketState]
)(using
    clock: Clock[F]
) extends MarketStateRepository[F]:
  override def delete(uid: UserId, cp: CurrencyPair): F[Unit] = Monad[F].unit
  override def deleteAll(uid: UserId): F[Unit]                = Monad[F].unit
  override def save(updated: MarketState): F[Boolean]         =
    clock.now.flatMap { now =>
      state.modify { s =>
        if (s.version != updated.version) (s, false)
        else (updated.copy(lastUpdatedAt = now, version = nextVersion(s)), true)
      }
    }
  override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState] =
    clock.now.flatMap { now =>
      state.updateAndGet(s => s.copy(currentPosition = position, lastUpdatedAt = now, version = nextVersion(s)))
    }
  private def nextVersion(s: MarketState): Option[Long]  = Some(s.version.fold(1L)(_ + 1))
  override def getAll(uid: UserId): F[List[MarketState]] =
    state.get.map(List(_))

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    state.get.map(Some(_))

object TestMarketService:
  def make[F[_]: {Concurrent, Clock, Logger}](initialState: MarketState, dispatcher: ActionDispatcher[F]): F[MarketService[F]] =
    Ref.of(initialState).flatMap(s => MarketService.make(TestMarketStateRepository(s), dispatcher))
