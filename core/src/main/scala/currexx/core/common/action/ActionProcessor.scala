package currexx.core.common.action

import cats.Monad
import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import currexx.core.monitor.MonitorService
import currexx.core.market.MarketService
import currexx.core.signal.SignalService
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
    private val marketService: MarketService[F]
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
        F.sleep(period) *> dispatcher.dispatch(Action.QueryMonitor(uid, mid))
      case Action.QueryMonitor(uid, mid) =>
        logger.info(s"querying monitor $mid") *> monitorService.query(uid, mid)
      case Action.ProcessMarketData(uid, data) =>
        logger.info(s"processing market data for ${data.currencyPair}") *> 
          marketService.processMarketData(uid, data) *> signalService.processMarketData(uid, data)
      case Action.ProcessSignal(signal) =>
        logger.info(s"processing submitted signal $signal") *> marketService.processSignal(signal)
      case Action.ProcessMarketState(state) =>
        logger.info(s"processing update market state $state")
    ).handleErrorWith {
      case error: AppError =>
        logger.warn(error)(s"domain error while processing action $action")
      case error =>
        logger.error(error)(s"unexpected error processing action $action") *>
          Temporal[F].sleep(1.second) *>
          dispatcher.dispatch(action)
    }
}

object ActionProcessor:
  def make[F[_]: Temporal: Logger](
      dispatcher: ActionDispatcher[F],
      monitorService: MonitorService[F],
      signalService: SignalService[F],
      marketService: MarketService[F]
  ): F[ActionProcessor[F]] =
    Monad[F].pure(LiveActionProcessor[F](dispatcher, monitorService, signalService, marketService))
