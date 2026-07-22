package currexx.backtest.services

import cats.data.NonEmptyList
import cats.effect.{Ref, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.{MarketMark, OrderStats, OrderStatsCollector, RiskSettings, TestSettings}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.common.logging.Logger
import currexx.core.market.MarketService
import currexx.core.signal.{SignalDetector, SignalService}
import currexx.core.trade.{TradeOrderPlacement, TradeService}
import currexx.domain.market.MarketTimeSeriesData
import fs2.{Pipe, Stream}
import kirill5k.common.syntax.time.*

import scala.concurrent.duration.*

final class TestServices[F[_]] private (
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val clients: TestClients[F],
    private val clock: TestClock[F],
    private val appState: ApplicationState[F]
)(using
    F: Temporal[F]
) {

  private val fetchTimeOffset: FiniteDuration = 100.seconds

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
              clients.data.setData(currentData)

            case Some(signalData) =>
              for
                currentBar    = currentData.prices.head
                executionBar  = currentBar.copy(close = currentBar.open)
                executionData = currentData.copy(prices = NonEmptyList(executionBar, currentData.prices.tail))
                executionTime = currentData.latestTime.plus(fetchTimeOffset)
                userId <- appState.userIdRef.get
                // Signals use the fully closed previous candle, while orders execute at the next
                // candle's open. This avoids filling at a close that is only known retrospectively.
                _ <- clients.data.setData(executionData)
                _ <- clock.setTime(executionTime)
                _ <- marketService.updateTimeState(userId, signalData)
                _ <- signalService.processMarketData(userId, signalData, signalDetector)
                _ <- collectPendingActions { case Action.ProcessSignals(uid, cp, signals) =>
                  marketService.processSignals(uid, cp, signals)
                }
                _ <- collectPendingActions { case Action.ProcessMarketStateUpdate(uid, cp) =>
                  marketService.getState(uid, cp).flatMap(tradeService.processMarketStateUpdate)
                }
                _ <- collectPendingActions { case Action.ProcessTradeOrderPlacement(top) =>
                  marketService.processTradeOrderPlacement(top)
                }
                // Keep the actual last close available for final mark-to-market accounting.
                _ <- clients.data.setData(currentData)
              yield ()
          }
        }
      }

  def getOrderStats(riskSettings: RiskSettings = RiskSettings()): F[OrderStats] =
    for
      orders     <- loadAllOrders
      latestData <- appState.dataRef.get
      finalMark = latestData.map { data =>
        MarketMark(
          price = BigDecimal(data.prices.head.close),
          observedAt = data.latestTime.plus(data.interval.toDuration + fetchTimeOffset)
        )
      }
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

      clock   = TestClock[F](appState.clockRef)
      clients = TestClients[F](TestBrokerClient[F], TestMarketDataClient[F](appState.dataRef))

      market <- MarketService.make[F](TestMarketStateRepository[F](appState.marketStateRef)(using Temporal[F], clock), dispatcher)

      tradeSettingsRepo = new TestTradeSettingsRepository[F](appState.tradeSettingsRef)
      tradeOrdersRepo   = new TestTradeOrderRepository[F](appState.tradeOrdersRef)
      orderStatusRepo   = new TestOrderStatusRepository[F]()
      trade <- TradeService.make[F](tradeSettingsRepo, tradeOrdersRepo, orderStatusRepo, clients.broker, clients.data, dispatcher)(using
        Temporal[F],
        clock
      )

      signalSettingsRepo = new TestSignalSettingsRepository[F](appState.signalSettingsRef)
      signal <- SignalService.make[F](TestSignalRepository[F], signalSettingsRepo, dispatcher)(using Temporal[F], clock)
    yield TestServices[F](signal, market, trade, clients, clock, appState)
