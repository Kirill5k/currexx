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
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val clients: TestClients[F],
    private val clock: TestClock[F],
    private val dispatcher: ActionDispatcher[F],
    private val appState: ApplicationState[F]
)(using
    F: Temporal[F]
) {

  def reset(newSettings: TestSettings): F[Unit] =
    appState.reset(newSettings)

  private def collectPendingActions(pf: PartialFunction[Action, F[Unit]]): F[Unit] =
    for
      actions <- dispatcher.pendingActions
      _       <- actions.collect(pf).sequence
    yield ()

  def processMarketData(signalDetector: SignalDetector): Pipe[F, MarketTimeSeriesData, Unit] =
    _.evalMap { data =>
      for
        userId <- appState.userIdRef.get
        _      <- clients.data.setData(data)
        _      <- clock.setTime(data.prices.head.time)
        _      <- marketService.updateTimeState(userId, data)
        _      <- signalService.processMarketData(userId, data, signalDetector)
        _      <- collectPendingActions { case Action.ProcessSignals(uid, cp, signals) =>
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
    appState.userIdRef.get.flatMap(userId => tradeService.getAllOrders(userId, SearchParams(None, None, None)))
}

object TestServices:
  def make[F[_]: Temporal](settings: TestSettings): F[TestServices[F]] =
    for
      appState   <- ApplicationState.make[F](settings)
      dispatcher <- ActionDispatcher.make[F](appState.dispatcherQueue)

      clock   = TestClock[F](appState.clockRef)
      clients = TestClients[F](TestBrokerClient[F], TestMarketDataClient[F](appState.dataRef))

      market <- MarketService.make[F](TestMarketStateRepository[F](appState.marketStateRef)(using Temporal[F], clock), dispatcher)

      tradeSettingsRepo = new TestTradeSettingsRepository[F](appState.tradeSettingsRef)
      tradeOrdersRepo   = new TestTradeOrderRepository[F](appState.tradeOrdersRef)
      trade <- TradeService.make[F](tradeSettingsRepo, tradeOrdersRepo, clients.broker, clients.data, dispatcher)(using Temporal[F], clock)

      signalSettingsRepo = new TestSignalSettingsRepository[F](appState.signalSettingsRef)
      signal <- SignalService.make[F](TestSignalRepository[F], signalSettingsRepo, dispatcher)(using Temporal[F], clock)
    yield TestServices[F](signal, market, trade, clients, clock, dispatcher, appState)
