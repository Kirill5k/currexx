package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit]
  def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit]
  def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Monad[F]
) extends MarketService[F] {
  override def getState(uid: UserId): F[List[MarketState]]                   = stateRepo.getAll(uid)
  override def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit] =
    stateRepo.deleteAll(uid) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseAllOpenOrders(uid)))

  override def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit] =
    stateRepo.delete(uid, cp) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseOpenOrders(uid, cp)))

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case enter: TradeOrder.Enter => Some(PositionState(enter.position, top.time, top.order.price))
      case _: TradeOrder.Exit      => None
    stateRepo.update(top.userId, top.order.currencyPair, position).void
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit] =
    stateRepo.find(uid, cp).flatMap { maybeState =>
      val currentProfile = maybeState.fold(MarketProfile())(_.profile)
      val updatedProfile = signals.foldLeft(currentProfile)(_.update(_))
      F.whenA(updatedProfile != currentProfile) {
        stateRepo
          .update(uid, cp, updatedProfile)
          .flatMap(state => dispatcher.dispatch(Action.ProcessMarketStateUpdate(state, currentProfile)))
      }
    }
}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
