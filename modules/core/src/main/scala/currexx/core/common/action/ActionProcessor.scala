package currexx.core.common.action

import cats.Monad
import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import currexx.core.monitor.MonitorService
import currexx.core.market.MarketService
import currexx.core.settings.SettingsService
import currexx.core.signal.SignalService
import currexx.core.trade.TradeService
import currexx.domain.errors.AppError
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

trait ActionProcessor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveActionProcessor[F[_]](
    private val dispatcher: ActionDispatcher[F],
    private val monitorService: MonitorService[F],
    private val signalService: SignalService[F],
    private val marketService: MarketService[F],
    private val tradeService: TradeService[F],
    private val settingsService: SettingsService[F]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends ActionProcessor[F] {
  override def run: Stream[F, Unit] =
    dispatcher.actions.map(a => Stream.eval(handleAction(a))).parJoinUnbounded

  private def handleAction(action: Action): F[Unit] =
    (action match
      case Action.RescheduleAllMonitors =>
        logger.info("rescheduling all monitors") *> monitorService.rescheduleAll
      case Action.ScheduleMonitor(uid, mid, period) =>
        F.sleep(period) *> logger.info(s"triggering monitor $uid/$mid") *> monitorService.triggerMonitor(uid, mid)
      case Action.FetchMarketData(uid, cps, interval) =>
        logger.info(s"fetching market data for $uid/$cps") *> tradeService.fetchMarketData(uid, cps, interval)
      case Action.ProcessMarketData(uid, data) =>
        logger.info(s"processing market data for $uid/${data.currencyPair}") *> signalService.processMarketData(uid, data)
      case Action.ProcessSignals(uid, cp, signals) =>
        logger.info(s"processing submitted signals for $uid/$cp") *> marketService.processSignals(uid, cp, signals)
      case Action.AssertProfit(uid, cps, limits) =>
        logger.info(s"verifying current position for $uid/$cps") *> tradeService.closeOrderIfProfitIsOutsideRange(uid, cps, limits)
      case Action.CloseAllOpenOrders(uid) =>
        logger.info(s"closing all opened orders for $uid") *> tradeService.closeOpenOrders(uid)
      case Action.CloseOpenOrders(uid, pair) =>
        logger.info(s"closing opened order for $uid/$pair currency pair") *> tradeService.closeOpenOrders(uid, pair)
      case Action.ProcessMarketStateUpdate(state, indicators) =>
        logger.info(s"processing market state update triggered by $indicators") *> tradeService.processMarketStateUpdate(state, indicators)
      case Action.ProcessTradeOrderPlacement(order) =>
        logger.info(s"processing trade order placement $order") *> marketService.processTradeOrderPlacement(order)
      case Action.SetupNewUser(uid) =>
        logger.info(s"setting up new user account for $uid") *> settingsService.createFor(uid)
    ).handleErrorWith {
      case error: AppError =>
        logger.warn(error)(s"domain error while processing action $action")
      case error =>
        logger.error(error)(s"unexpected error while processing action $action") *>
          F.sleep(1.second) *> dispatcher.dispatch(action)
    }
}

object ActionProcessor:
  def make[F[_]: {Temporal, Logger}](
      dispatcher: ActionDispatcher[F],
      monitorService: MonitorService[F],
      signalService: SignalService[F],
      marketService: MarketService[F],
      tradeService: TradeService[F],
      settingsService: SettingsService[F]
  ): F[ActionProcessor[F]] =
    Monad[F].pure(LiveActionProcessor[F](dispatcher, monitorService, signalService, marketService, tradeService, settingsService))
