package currexx.core.market

import cats.MonadThrow
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.foldable.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.{BrokerPosition, TradeOrderPlacement}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData, TradeOrder}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def getState(uid: UserId, cp: CurrencyPair): F[MarketState]
  def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit]
  def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit]
  def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit]
  def processManualSignal(signal: Signal): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]
  def updateTimeState(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: MonadThrow[F],
    clock: Clock[F],
    logger: Logger[F]
) extends MarketService[F] {
  import LiveMarketService.MaxSaveAttempts

  override def getState(uid: UserId): F[List[MarketState]]             = stateRepo.getAll(uid)
  override def getState(uid: UserId, cp: CurrencyPair): F[MarketState] =
    stateRepo.find(uid, cp).flatMap(F.fromOption(_, AppError.MissingMarketState(cp)))
  override def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit] =
    stateRepo.deleteAll(uid) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseAllOpenOrders(uid)))

  override def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit] =
    stateRepo.delete(uid, cp) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseOpenOrders(uid, cp)))

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] =
    top.order match
      case _: TradeOrder.Exit      => stateRepo.update(top.userId, top.order.currencyPair, None).void
      case enter: TradeOrder.Enter =>
        modifyState(top.userId, enter.currencyPair) { state =>
          val position = positionAfterEntry(state.currentPosition, enter, top)
          Option.when(position != state.currentPosition)(state.copy(currentPosition = position))
        }.void

  // A filled entry takes the broker's position as a whole, so applying the same placement again changes nothing.
  // An entry that has not filled yet leaves a position on its side as it was, or opens one without a price.
  private def positionAfterEntry(
      current: Option[PositionState],
      enter: TradeOrder.Enter,
      top: TradeOrderPlacement
  ): Option[PositionState] = {
    def onSide(side: TradeOrder.Position): Option[PositionState]                        = current.filter(_.position == side)
    def filled(side: TradeOrder.Position, openPrice: Option[BigDecimal]): PositionState =
      val filledAt = top.executions.map(_.time).minOption.getOrElse(top.time)
      PositionState(side, onSide(side).fold(filledAt)(_.openedAt), openPrice)

    top.brokerPosition match
      case Some(BrokerPosition.Flat)              => None
      case Some(BrokerPosition.Open(side, price)) => Some(filled(side, Some(price)))
      case Some(BrokerPosition.Unknown)           => Some(filled(enter.position, None))
      case None                                   => Some(onSide(enter.position).getOrElse(PositionState(enter.position, top.time)))
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit] =
    signals.headOption.traverse_ { firstSignal =>
      modifyState(uid, cp)(_.applyCandleSignals(signals, firstSignal.time)).flatMap(dispatchIfProfileChanged)
    }

  override def processManualSignal(signal: Signal): F[Unit] =
    modifyState(signal.userId, signal.currencyPair)(_.applyManualSignal(signal)).flatMap(dispatchIfProfileChanged)

  override def updateTimeState(uid: UserId, data: MarketTimeSeriesData): F[Unit] = {
    val interval         = data.interval.toDuration
    val marketClosureGap = data.prices.tail.headOption
      .map(_.time.durationBetween(data.latestTime))
      .filter(_ > interval * 2)
      .map(_ - interval)
    modifyState(uid, data.currencyPair)(_.applyTimeState(data.latestTime, marketClosureGap)).flatMap { change =>
      marketClosureGap.filter(_ => change.isDefined).traverse_ { gap =>
        logger.info(s"shifted state by ${gap.toHours}h to adjust for market close time for $uid/${data.currencyPair}")
      }
    }
  }

  private def dispatchIfProfileChanged(change: Option[(MarketState, MarketState)]): F[Unit] =
    change.filter((before, after) => before.profile != after.profile).traverse_ { (_, after) =>
      dispatcher.dispatch(Action.ProcessMarketStateUpdate(after.userId, after.currencyPair))
    }

  // Returns the state before and after the accepted change, or None when `f` decides there is nothing to change.
  private def modifyState(uid: UserId, cp: CurrencyPair)(f: MarketState => Option[MarketState]): F[Option[(MarketState, MarketState)]] = {
    def attempt(n: Int): F[Option[(MarketState, MarketState)]] =
      stateRepo.find(uid, cp).flatMap(_.fold(clock.now.map(MarketState.initial(uid, cp, _)))(F.pure)).flatMap { current =>
        f(current) match
          case None          => F.pure(None)
          case Some(updated) =>
            stateRepo.save(updated).flatMap {
              case true                         => F.pure(Some(current -> updated))
              case false if n < MaxSaveAttempts => attempt(n + 1)
              // Not an AppError, so ActionProcessor retries the whole action with a backoff.
              case false => F.raiseError(new IllegalStateException(s"market state for $uid/$cp kept changing after $n attempts"))
            }
      }
    attempt(1)
  }
}

private object LiveMarketService:
  val MaxSaveAttempts = 5

object MarketService:
  def make[F[_]: {MonadThrow, Clock, Logger}](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    LiveMarketService[F](stateRepo, dispatcher).pure[F]
