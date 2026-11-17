package currexx.core.market

import cats.MonadThrow
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.market.db.MarketStateRepository
import currexx.core.settings.StrategyRuntime
import currexx.core.signal.SignalBatch
import currexx.core.trade.{BrokerPosition, TradeOrderPlacement}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def getState(uid: UserId, cp: CurrencyPair): F[MarketState]
  def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit]
  def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit]
  def processSignals(uid: UserId, cp: CurrencyPair, batch: SignalBatch): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]
  def reconcileEntryPrice(state: MarketState, brokerPosition: BrokerPosition): F[MarketState]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F],
    private val runtime: StrategyRuntime[F]
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

  // The broker's position describes the one held when `state` was read, so its price is saved only against that snapshot.
  // Only a missing price is filled in: a flat broker position may still have an entry pending, so it clears nothing.
  override def reconcileEntryPrice(state: MarketState, brokerPosition: BrokerPosition): F[MarketState] = {
    def warnIfUnpriced(s: MarketState): F[Unit] =
      s.currentPosition.filter(_.openPrice.isEmpty).traverse_ { pos =>
        logger.warn(
          s"entry price of the ${pos.position} position on ${s.userId}/${s.currencyPair} is unknown, price-distance exits cannot trigger"
        )
      }

    (state.currentPosition, brokerPosition) match
      case (Some(pos), BrokerPosition.Open(side, price)) if pos.openPrice.isEmpty && pos.position == side =>
        val repaired = state.copy(currentPosition = Some(pos.copy(openPrice = Some(price))))
        stateRepo.save(repaired).flatMap {
          case true  => F.pure(repaired)
          case false => getState(state.userId, state.currencyPair).flatTap(warnIfUnpriced)
        }
      case _ => warnIfUnpriced(state).as(state)
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, batch: SignalBatch): F[Unit] =
    modifyStateWithResult(uid, cp) { state =>
      runtime.load(uid).flatMap { config =>
        if config.revision != batch.revision then
          logger
            .info(
              s"rejecting signal batch for $uid/$cp at ${batch.candleTime}: " +
                s"configuration revision ${batch.revision} is no longer active (${config.revision})"
            )
            .as(None)
        else if config.sources != batch.sources then
          logger.info(s"rejecting signal batch for $uid/$cp at ${batch.candleTime}: indicator definitions changed").as(None)
        else
          val transition = ProfileTransition(state, batch)
          F.pure(Some(transition.state -> transition.outcome))
      }
    }.flatMap(_.traverse_ { (_, _, outcome) =>
      outcome match
        case ProfileTransition.Outcome.Changed   => dispatcher.dispatch(Action.ProcessMarketStateUpdate(uid, cp))
        case ProfileTransition.Outcome.Unchanged => F.unit
        case ProfileTransition.Outcome.Skipped   =>
          F.whenA(batch.signals.nonEmpty)(
            logger.info(
              s"skipping signal application for $uid/$cp at ${batch.candleTime}: candle was already applied or is older than the latest poll"
            )
          )
    })

  // Returns the state before and after the accepted change, or None when `f` decides there is nothing to change.
  private def modifyState(uid: UserId, cp: CurrencyPair)(f: MarketState => Option[MarketState]): F[Option[(MarketState, MarketState)]] =
    modifyStateWithResult(uid, cp)(s => F.pure(f(s).map(_ -> ()))).map(_.map((before, after, _) => before -> after))

  private def modifyStateWithResult[A](uid: UserId, cp: CurrencyPair)(
      f: MarketState => F[Option[(MarketState, A)]]
  ): F[Option[(MarketState, MarketState, A)]] =
    def attempt(n: Int): F[Option[(MarketState, MarketState, A)]] =
      stateRepo.find(uid, cp).flatMap(_.fold(clock.now.map(MarketState.initial(uid, cp, _)))(F.pure)).flatMap { current =>
        f(current).flatMap {
          case None                                         => F.pure(None)
          case Some((updated, value)) if updated == current => F.pure(Some((current, current, value)))
          case Some((updated, value))                       =>
            stateRepo.save(updated).flatMap {
              case true                         => F.pure(Some((current, updated, value)))
              case false if n < MaxSaveAttempts => attempt(n + 1)
              case false => F.raiseError(new IllegalStateException(s"market state for $uid/$cp kept changing after $n attempts"))
            }
        }
      }
    attempt(1)

}

private object LiveMarketService:
  val MaxSaveAttempts = 5

object MarketService:
  def make[F[_]: {MonadThrow, Clock, Logger}](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F],
      runtime: StrategyRuntime[F]
  ): F[MarketService[F]] =
    LiveMarketService[F](stateRepo, dispatcher, runtime).pure[F]
