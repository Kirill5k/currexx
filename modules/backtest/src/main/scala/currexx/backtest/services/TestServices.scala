package currexx.backtest.services

import cats.effect.{Ref, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.{OrderStats, OrderStatsCollector, RiskSettings, TestSettings}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.common.logging.Logger
import currexx.core.market.MarketService
import currexx.core.signal.{SignalDetector, SignalService}
import currexx.core.trade.{TradeOrderPlacement, TradeService}
import currexx.domain.market.MarketTimeSeriesData
import fs2.{Pipe, Stream}

final class TestServices[F[_]] private (
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val appState: ApplicationState[F]
)(using
    F: Temporal[F]
) {

  def reset(newSettings: TestSettings): F[Unit] =
    appState.reset(newSettings)

  private def collectPendingActions(pf: PartialFunction[Action, F[Unit]]): F[Unit] =
    for
      actions <- appState.dispatcherQueue.tryTakeN(None)
      _       <- actions.collect(pf).sequence
    yield ()

  def processMarketData(signalDetector: SignalDetector): Pipe[F, MarketTimeSeriesData, Unit] =
    input =>
      Stream.eval(Ref.of[F, Option[MarketTimeSeriesData]](None)).flatMap { previousData =>
        input.evalMap { currentData =>
          previousData.getAndSet(Some(currentData)).flatMap {
            case None =>
              // The first window only primes the simulator; there is no next-bar fill for it yet.
              F.unit

            case Some(signalData) =>
              for
                _      <- appState.prepareExecution(currentData)
                userId <- appState.userIdRef.get
                _      <- marketService.updateTimeState(userId, signalData)
                _      <- signalService.processMarketData(userId, signalData, signalDetector)
                _      <- collectPendingActions { case Action.ProcessSignals(uid, cp, signals) =>
                  marketService.processSignals(uid, cp, signals)
                }
                _ <- collectPendingActions { case Action.ProcessMarketStateUpdate(uid, cp) =>
                  marketService.getState(uid, cp).flatMap(tradeService.processMarketStateUpdate)
                }
                _ <- collectPendingActions { case Action.ProcessTradeOrderPlacement(top) =>
                  marketService.processTradeOrderPlacement(top)
                }
              yield ()
          }
        }
      }

  def getOrderStats(riskSettings: RiskSettings = RiskSettings()): F[OrderStats] =
    for
      orders    <- loadAllOrders
      finalMark <- appState.finalMarkRef.get
    yield OrderStatsCollector.collect(orders, finalMark, riskSettings)

  private def loadAllOrders: F[List[TradeOrderPlacement]] =
    appState.userIdRef.get.flatMap(userId => tradeService.getAllOrders(userId, SearchParams(None, None, None)))

  def getAllOrders: F[List[TradeOrderPlacement]] = loadAllOrders
}

object TestServices:
  def make[F[_]: Temporal](settings: TestSettings): F[TestServices[F]] =
    given Logger[F] = Logger.noop[F]
    for
      appState   <- ApplicationState.make[F](settings)
      dispatcher <- ActionDispatcher.make[F](appState.dispatcherQueue)

      clock = TestClock[F](appState.clockRef)

      market <- MarketService.make[F](
        stateRepo = TestMarketStateRepository[F](appState.marketStateRef)(using Temporal[F], clock),
        dispatcher = dispatcher
      )

      clients = TestClients[F](TestBrokerClient[F], TestMarketDataClient[F](appState.dataRef))

      trade <- TradeService.make[F](
        settingsRepo = new TestTradeSettingsRepository[F](appState.tradeSettingsRef),
        orderRepository = new TestTradeOrderRepository[F](appState.tradeOrdersRef),
        orderStatusRepository = new TestOrderStatusRepository[F](),
        clients.broker,
        clients.data,
        dispatcher
      )(using
        Temporal[F],
        clock
      )

      signalSettingsRepo = new TestSignalSettingsRepository[F](appState.signalSettingsRef)
      signal <- SignalService.make[F](TestSignalRepository[F], signalSettingsRepo, dispatcher)(using Temporal[F], clock)
    yield TestServices[F](signal, market, trade, appState)
