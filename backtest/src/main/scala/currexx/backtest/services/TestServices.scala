package currexx.backtest.services

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
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
    private val clients: TestClients[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Async[F]
) {

  private def collectPendingActions(pf: PartialFunction[Action, F[Unit]]): F[Unit] =
    for
      actions <- dispatcher.pendingActions
      _       <- actions.collect(pf).sequence
    yield ()

  def processMarketData: Pipe[F, MarketTimeSeriesData, Unit] = { dataStream =>
    dataStream.evalMap { data =>
      for
        _ <- clients.data.setData(data)
        _ <- signalService.processMarketData(settings.userId, data)
        _ <- collectPendingActions { case Action.ProcessSignals(uid, cp, signals) =>
          marketService.processSignals(uid, cp, signals)
        }
        _ <- collectPendingActions { case Action.ProcessMarketStateUpdate(state, triggeredBy) =>
          tradeService.processMarketStateUpdate(state, triggeredBy)
        }
        _ <- collectPendingActions { case Action.ProcessTradeOrderPlacement(top) =>
          marketService.processTradeOrderPlacement(top)
        }
      yield ()
    }
  }

  def getAllOrders: F[List[TradeOrderPlacement]] =
    tradeService.getAllOrders(settings.userId, SearchParams(None, None, None))
}

object TestServices:
  def make[F[_]: Async](settings: TestSettings): F[TestServices[F]] =
    for
      dispatcher <- ActionDispatcher.make[F]
      clients    <- TestClients.make[F]
      market     <- TestMarketService.make[F](settings.marketState, dispatcher)
      trade      <- TestTradeService.make[F](settings.trade, clients, dispatcher)
      signal     <- TestSignalService.make[F](settings.signal, dispatcher)
    yield TestServices[F](settings, signal, market, trade, clients, dispatcher)
