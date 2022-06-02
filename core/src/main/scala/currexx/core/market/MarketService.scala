package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.time.*
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{Indicator, MarketTimeSeriesData, TradeOrder}
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def clearState(uid: UserId): F[Unit]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]
  def processSignal(signal: Signal): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Monad[F]
) extends MarketService[F] {
  override def getState(uid: UserId): F[List[MarketState]] = stateRepo.getAll(uid)
  override def clearState(uid: UserId): F[Unit]            = stateRepo.deleteAll(uid) >> dispatcher.dispatch(Action.CloseAllOpenOrders(uid))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    stateRepo.update(uid, data.currencyPair, data.prices.head).void

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case enter: TradeOrder.Enter => Some(enter.position)
      case TradeOrder.Exit         => None
    stateRepo.update(top.userId, top.currencyPair, position.map(p => PositionState(p, top.time, top.currentPrice))).void
  }

  override def processSignal(signal: Signal): F[Unit] =
    stateRepo
      .find(signal.userId, signal.currencyPair)
      .flatMap { state =>
        val signals           = state.fold(Map.empty[String, List[IndicatorState]])(_.signals)
        val indicatorStates   = signals.getOrElse(signal.triggeredBy.kind, Nil)
        val newIndicatorState = IndicatorState(signal.condition, signal.time, signal.triggeredBy)
        val updatedIndicatorStates =
          if (indicatorStates.isEmpty) List(newIndicatorState)
          else if (indicatorStates.head.time.hasSameDateAs(signal.time)) newIndicatorState :: indicatorStates.tail
          else newIndicatorState :: indicatorStates
        stateRepo
          .update(signal.userId, signal.currencyPair, signals + (signal.triggeredBy.kind -> updatedIndicatorStates.take(10)))
          .map(s => Option.when(updatedIndicatorStates != indicatorStates)(s))
      }
      .flatMap {
        case Some(state) => dispatcher.dispatch(Action.ProcessMarketStateUpdate(state, signal.triggeredBy))
        case None        => F.unit
      }

}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
