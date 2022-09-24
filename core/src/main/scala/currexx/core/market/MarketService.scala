package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.time.*
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, Indicator, MarketTimeSeriesData, TradeOrder}
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
  override def getState(uid: UserId): F[List[MarketState]] = stateRepo.getAll(uid)
  override def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit] =
    stateRepo.deleteAll(uid) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseAllOpenOrders(uid)))

  override def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit] =
    stateRepo.delete(uid, cp) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseOpenOrders(uid, cp)))

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case enter: TradeOrder.Enter => Some(PositionState(enter.position, top.time, top.price))
      case TradeOrder.Exit         => None
    stateRepo.update(top.userId, top.currencyPair, position).void
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit] =
    stateRepo
      .find(uid, cp)
      .flatMap { state =>
        val existingSignals = state.fold(Map.empty[String, List[IndicatorState]])(_.signals)

        val updatedSignals = signals.foldLeft(existingSignals) { (current, signal) =>
          val indicatorStates   = current.getOrElse(signal.triggeredBy.kind, Nil)
          val newIndicatorState = IndicatorState(signal.condition, signal.time, signal.triggeredBy)
          val updatedIndicatorStates =
            if (indicatorStates.isEmpty) List(newIndicatorState)
            else if (indicatorStates.head.time.hasSameDateAs(signal.time)) newIndicatorState :: indicatorStates.tail
            else newIndicatorState :: indicatorStates.take(5)
          current + (signal.triggeredBy.kind -> updatedIndicatorStates)
        }

        if (existingSignals == updatedSignals) F.pure(None)
        else stateRepo.update(uid, cp, updatedSignals).map(Some(_))
      }
      .flatMap {
        case Some(state) => dispatcher.dispatch(Action.ProcessMarketStateUpdate(state, signals.map(_.triggeredBy)))
        case None        => F.unit
      }
}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
