package currexx.backtest.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.backtest.TestSettings
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.signal.SignalService
import currexx.core.market.MarketService
import currexx.core.trade.{TradeOrderPlacement, TradeService}
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId
import fs2.{Pipe, Stream}

final class TestServices[F[_]] private (
    private val settings: TestSettings,
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

  def processMarketData: Pipe[F, MarketTimeSeriesData, Unit] = { dataStream =>
    for
      data <- dataStream
      _    <- Stream.eval(marketService.processMarketData(settings.userId, data) >> signalService.processMarketData(settings.userId, data))
      _ <- pendingActions.evalMap {
        case Action.ProcessSignal(signal) => marketService.processSignal(signal)
        case _                            => F.unit
      }
      _ <- pendingActions.evalMap {
        case Action.ProcessMarketStateUpdate(state, triggeredBy) => tradeService.processMarketStateUpdate(state, triggeredBy)
        case _                                                   => F.unit
      }
      _ <- pendingActions.evalMap {
        case Action.ProcessTradeOrderPlacement(top) => marketService.processTradeOrderPlacement(top)
        case _                                      => F.unit
      }
    yield ()
  }

  def getAllOrders: F[List[TradeOrderPlacement]] =
    tradeService.getAllOrders(settings.userId, SearchParams(None, None, None))
}

object TestServices:
  def make[F[_]: Async](settings: TestSettings): F[TestServices[F]] =
    for
      dispatcher <- ActionDispatcher.make[F]
      market     <- TestMarketService.make[F](settings.marketState, dispatcher)
      trade      <- TestTradeService.make[F](settings.trade, dispatcher)
      signal     <- TestSignalService.make[F](settings.signal, dispatcher)
    yield TestServices[F](settings, signal, market, trade, dispatcher)
