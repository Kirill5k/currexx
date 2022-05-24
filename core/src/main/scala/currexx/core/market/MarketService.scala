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
  override def clearState(uid: UserId): F[Unit]            = stateRepo.deleteAll(uid)

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    stateRepo.update(uid, data.currencyPair, data.prices.head).void

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case TradeOrder.Enter(position, _, _, _, _) => Some(position)
      case TradeOrder.Exit                        => None
    stateRepo.update(top.userId, top.currencyPair, position).void
  }

  override def processSignal(signal: Signal): F[Unit] =
    stateRepo
      .find(signal.userId, signal.currencyPair)
      .flatMap { state =>
        val signals         = state.fold(Map.empty[Indicator, List[IndicatorState]])(_.signals)
        val indicatorStates = signals.getOrElse(signal.indicator, Nil)
        val updatedIndicatorStates =
          if (indicatorStates.isEmpty) List(IndicatorState(signal.condition, signal.time))
          else if (indicatorStates.head.time.hasSameDateAs(signal.time))
            IndicatorState(signal.condition, signal.time) :: indicatorStates.tail
          else IndicatorState(signal.condition, signal.time) :: indicatorStates
        stateRepo
          .update(signal.userId, signal.currencyPair, signals + (signal.indicator -> updatedIndicatorStates.take(10)))
          .map(s => Option.when(updatedIndicatorStates != indicatorStates)(s))
      }
      .flatMap {
        case Some(state) => dispatcher.dispatch(Action.ProcessMarketState(state, signal.indicator))
        case None        => F.unit
      }

}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
