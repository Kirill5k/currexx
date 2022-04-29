package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.time.*
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.domain.market.{Indicator, MarketTimeSeriesData}
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]
  def processSignal(signal: Signal): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Monad[F]
) extends MarketService[F] {
  override def getState(uid: UserId): F[List[MarketState]] = stateRepo.getAll(uid)

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    stateRepo.update(uid, data.currencyPair, data.prices.head).void

  override def processSignal(signal: Signal): F[Unit] =
    stateRepo
      .find(signal.userId, signal.currencyPair)
      .flatMap { state =>
        val signals        = state.fold(Map.empty[Indicator, List[IndicatorState]])(_.signals)
        val indicatorState = signals.getOrElse(signal.indicator, Nil)
        val updatedIndicatorStates =
          if (indicatorState.isEmpty) List(IndicatorState(signal.condition, signal.time))
          else if (indicatorState.head.time.hasSameDateAs(signal.time)) IndicatorState(signal.condition, signal.time) :: indicatorState.tail
          else IndicatorState(signal.condition, signal.time) :: indicatorState
        stateRepo
          .update(signal.userId, signal.currencyPair, signals + (signal.indicator -> updatedIndicatorStates.take(10)))
      }
      .flatMap(state => dispatcher.dispatch(Action.ProcessMarketState(state)))

}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
