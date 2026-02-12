package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData, TradeOrder}
import currexx.domain.user.UserId
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit]
  def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit]
  def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]
  def updateTimeState(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Monad[F],
    logger: Logger[F]
) extends MarketService[F] {
  override def getState(uid: UserId): F[List[MarketState]]                   = stateRepo.getAll(uid)
  override def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit] =
    stateRepo.deleteAll(uid) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseAllOpenOrders(uid)))

  override def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit] =
    stateRepo.delete(uid, cp) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseOpenOrders(uid, cp)))

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case enter: TradeOrder.Enter => Some(PositionState(enter.position, top.time))
      case _: TradeOrder.Exit      => None
    stateRepo.update(top.userId, top.order.currencyPair, position).void
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit] =
    stateRepo.find(uid, cp).flatMap { maybeState =>
      val currentProfile = maybeState.fold(MarketProfile())(_.profile)
      val updatedProfile = signals.foldLeft(currentProfile)(_.update(_))
      F.whenA(updatedProfile != currentProfile) {
        stateRepo
          .update(uid, cp, updatedProfile)
          .flatMap(state => dispatcher.dispatch(Action.ProcessMarketStateUpdate(state, currentProfile)))
      }
    }

  override def updateTimeState(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    data.prices.toList match
      case latestCandle :: previousCandle :: _ =>
        val cp      = data.currencyPair
        val timeGap = previousCandle.time.durationBetween(latestCandle.time)
        F.whenA(timeGap > (data.interval.toDuration * 2)) {
          val marketClosureGap = timeGap - data.interval.toDuration
          stateRepo.find(uid, cp).flatMap {
            case Some(previousState) =>
              val shiftedProfile = previousState.profile.copy(
                trend = previousState.profile.trend.map(s => s.copy(confirmedAt = s.confirmedAt.plus(marketClosureGap))),
                momentum = previousState.profile.momentum.map(s => s.copy(confirmedAt = s.confirmedAt.plus(marketClosureGap))),
                volatility = previousState.profile.volatility.map(s => s.copy(confirmedAt = s.confirmedAt.plus(marketClosureGap))),
                crossover = None,
                lastBandCrossing = None,
                lastPriceLineCrossing = None
              )
              val shiftedPosition = previousState.currentPosition.map(p => p.copy(openedAt = p.openedAt.plus(marketClosureGap)))
              logger.info(s"shifted state by ${marketClosureGap.toHours}h to adjust for market close time for $uid/$cp") >>
                stateRepo.update(uid, cp, shiftedProfile, shiftedPosition).void
            case None => F.unit
          }
        }
      case _ => F.unit
  }
}

object MarketService:
  def make[F[_]: {Monad, Logger}](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
