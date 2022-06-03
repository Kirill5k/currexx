package currexx.backtest.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.{SignalService, SignalSettings}
import currexx.core.market.{MarketService, MarketState}
import currexx.core.trade.{TradeOrderPlacement, TradeService, TradeSettings}
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId
import fs2.Stream

final class TestServices[F[_]] private (
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Async[F]
) {

  private def pendingActions: Stream[F, Action] =
    Stream
      .eval(dispatcher.numberOfPendingActions)
      .flatMap {
        case 0 => Stream.empty
        case n => dispatcher.actions.take(n)
      }

  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    (Stream.eval(marketService.processMarketData(uid, data) >> signalService.processMarketData(uid, data)) >>
      pendingActions.evalMap {
        case Action.ProcessSignal(signal) => marketService.processSignal(signal)
        case _                            => F.unit
      } >>
      pendingActions.evalMap {
        case Action.ProcessMarketStateUpdate(state, triggeredBy) => tradeService.processMarketStateUpdate(state, triggeredBy)
        case _                                                   => F.unit
      } >>
      pendingActions.evalMap {
        case Action.ProcessTradeOrderPlacement(top) => marketService.processTradeOrderPlacement(top)
        case _                                      => F.unit
      }).compile.drain

  def getAllOrders(uid: UserId): F[List[TradeOrderPlacement]] =
    tradeService.getAllOrders(uid)
}

object TestServices {
  def make[F[_]: Async](
      initialMarketState: MarketState,
      initialTradeSettings: TradeSettings,
      initialSignalSettings: SignalSettings
  ): F[TestServices[F]] =
    for
      dispatcher <- ActionDispatcher.make[F]
      market     <- TestMarketService.make[F](initialMarketState, dispatcher)
      trade      <- TestTradeService.make[F](initialTradeSettings, dispatcher)
      signal     <- TestSignalService.make[F](initialSignalSettings, dispatcher)
    yield TestServices[F](signal, market, trade, dispatcher)
}
