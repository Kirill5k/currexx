package currexx.core.market

import cats.Monad
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.Signal
import currexx.core.market.db.MarketStateRepository
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.Condition
import currexx.domain.user.UserId

trait MarketService[F[_]]:
  def getState(uid: UserId): F[List[MarketState]]
  def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit]
  def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit]
  def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit]
  def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit]

final private class LiveMarketService[F[_]](
    private val stateRepo: MarketStateRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Monad[F]
) extends MarketService[F] {
  override def getState(uid: UserId): F[List[MarketState]]                   = stateRepo.getAll(uid)
  override def clearState(uid: UserId, closePendingOrders: Boolean): F[Unit] =
    stateRepo.deleteAll(uid) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseAllOpenOrders(uid)))

  override def clearState(uid: UserId, cp: CurrencyPair, closePendingOrders: Boolean): F[Unit] =
    stateRepo.delete(uid, cp) >> F.whenA(closePendingOrders)(dispatcher.dispatch(Action.CloseOpenOrders(uid, cp)))

  override def processTradeOrderPlacement(top: TradeOrderPlacement): F[Unit] = {
    val position = top.order match
      case enter: TradeOrder.Enter => Some(PositionState(enter.position, top.time, top.order.price))
      case _: TradeOrder.Exit      => None
    stateRepo.update(top.userId, top.order.currencyPair, position).void
  }

  override def processSignals(uid: UserId, cp: CurrencyPair, signals: List[Signal]): F[Unit] =
    stateRepo.find(uid, cp).flatMap { maybeState =>

      val currentProfile = maybeState.fold(MarketProfile())(_.profile)
      val updatedProfile = signals.foldLeft(currentProfile)(updateProfileWithCondition)
      F.whenA(updatedProfile != currentProfile) {
        stateRepo
          .update(uid, cp, updatedProfile)
          .flatMap(state => dispatcher.dispatch(Action.ProcessMarketStateUpdate(state, currentProfile)))
      }
    }

  private def updateProfileWithCondition(profile: MarketProfile, signal: Signal): MarketProfile =
    signal.condition match {
      // --- Trend Signal ---
      case Condition.TrendDirectionChange(_, to, _) =>
        // A trend change occurred. Create a new TrendState.
        val newTrendState = TrendState(
          direction = to,
          confirmedAt = signal.time
        )
        profile.copy(trend = Some(newTrendState))

      // --- Crossover Signal ---
      case Condition.LinesCrossing(direction) =>
        // A crossover occurred. Create a new CrossoverState.
        val newCrossoverState = CrossoverState(
          direction = direction,
          confirmedAt = signal.time
        )
        profile.copy(crossover = Some(newCrossoverState))

      // --- Momentum Signals (Threshold Crossing) ---
      case Condition.AboveThreshold(_, value) =>
        // The oscillator crossed the upper boundary.
        // We are now in the Overbought zone.
        val newMomentumState = MomentumState(
          zone = MomentumZone.Overbought,
          confirmedAt = signal.time
        )
        profile.copy(
          momentum = Some(newMomentumState),
          lastMomentumValue = Some(value.toDouble) // Update the latest raw value
        )

      case Condition.BelowThreshold(_, value) =>
        // The oscillator crossed the lower boundary.
        // We are now in the Oversold zone.
        val newMomentumState = MomentumState(
          zone = MomentumZone.Oversold,
          confirmedAt = signal.time
        )
        profile.copy(
          momentum = Some(newMomentumState),
          lastMomentumValue = Some(value.toDouble) // Update the latest raw value
        )

      // --- Volatility Signals (e.g., from Keltner Channel) ---
      // Here we just pass the condition through. A more advanced system might create a VolatilityState.
      case _ @ (_: Condition.UpperBandCrossing | _: Condition.LowerBandCrossing) =>
        // This part of the profile could be enhanced further, but for now, we just note the event.
        // Let's assume the `VolatilityState` logic is not yet fully implemented.
        // For now, we can just update the last known value from a related indicator if available.
        // This part is highly dependent on how you define volatility signals.
        // As a placeholder, we do nothing with the state, just the value if we had it.
        profile // No change to the profile state, maybe just update a raw value if one was passed.

      // --- Composite Signal ---
      case Condition.Composite(conditions) =>
        // A composite signal is just a bundle of other signals.
        // We recursively fold over its inner conditions to update the profile.
        // This ensures that if a composite contains a TrendChange and a ThresholdCrossing,
        // BOTH the `trend` and `momentum` fields of the profile get updated correctly.
        conditions.foldLeft(profile) { (currentProfile, innerCondition) =>
          // To properly update, the inner call also needs the signal context.
          // This highlights a design choice: for simplicity here, we assume the composite's
          // top-level signal context (like time) applies to all children.
          val pseudoSignal = signal.copy(condition = innerCondition)
          updateProfileWithCondition(currentProfile, pseudoSignal)
        }
    }
}

object MarketService:
  def make[F[_]: Monad](
      stateRepo: MarketStateRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[MarketService[F]] =
    Monad[F].pure(LiveMarketService[F](stateRepo, dispatcher))
