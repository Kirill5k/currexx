package currexx.backtest.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.TestSettings
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.market.MarketService
import currexx.core.signal.{SignalDetector, SignalService}
import currexx.core.trade.{TradeOrderPlacement, TradeService}
import currexx.domain.market.MarketTimeSeriesData
import fs2.Pipe

final class TestServices[F[_]] private (
    private val settings: TestSettings,
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val clients: TestClients[F],
    private val clock: TestClock[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Temporal[F]
) {

  private def collectPendingActions(pf: PartialFunction[Action, F[Unit]]): F[Unit] =
    for
      actions <- dispatcher.pendingActions
      _       <- actions.collect(pf).sequence
    yield ()

  def processMarketData(signalDetector: SignalDetector): Pipe[F, MarketTimeSeriesData, Unit] =
    _.evalMap { data =>
      for
        _ <- clients.data.setData(data)
        _ <- clock.setTime(data.prices.head.time)
        _ <- marketService.updateTimeState(settings.userId, data)
        _ <- signalService.processMarketData(settings.userId, data, signalDetector)
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

  def getAllOrders: F[List[TradeOrderPlacement]] =
    tradeService.getAllOrders(settings.userId, SearchParams(None, None, None))
}

object TestServices:
  def make[F[_]: Temporal](settings: TestSettings): F[TestServices[F]] =
    for
      dispatcher <- ActionDispatcher.make[F]
      clock      <- TestClock.make[F]
      clients    <- TestClients.make[F]
      market     <- TestMarketService.make[F](settings.marketState, dispatcher)
      trade      <- TestTradeService.make[F](settings.trade, clients, dispatcher)(using Temporal[F], clock)
      signal     <- TestSignalService.make[F](settings.signal, dispatcher)
    yield TestServices[F](settings, signal, market, trade, clients, clock, dispatcher)
