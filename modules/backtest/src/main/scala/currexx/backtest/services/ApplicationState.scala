package currexx.backtest.services

import cats.Monad
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.backtest.TestSettings
import currexx.core.common.action.Action
import currexx.core.market.MarketState
import currexx.core.settings.{SignalSettings, TradeSettings}
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId

import java.time.Instant
import scala.collection.mutable.ListBuffer

final class ApplicationState[F[_]](
    val marketStateRef: Ref[F, MarketState],
    val tradeSettingsRef: Ref[F, TradeSettings],
    val tradeOrdersRef: Ref[F, ListBuffer[TradeOrderPlacement]],
    val signalSettingsRef: Ref[F, SignalSettings],
    val clockRef: Ref[F, Option[Instant]],
    val dataRef: Ref[F, Option[MarketTimeSeriesData]],
    val dispatcherQueue: Queue[F, Action],
    val userIdRef: Ref[F, UserId]
)(using F: Monad[F]) {

  def reset(newSettings: TestSettings): F[Unit] =
    for
      _ <- marketStateRef.set(newSettings.marketState)
      _ <- tradeSettingsRef.set(newSettings.trade)
      _ <- tradeOrdersRef.set(ListBuffer.empty[TradeOrderPlacement])
      _ <- signalSettingsRef.set(newSettings.signal)
      _ <- clockRef.set(None)
      _ <- dataRef.set(None)
      _ <- dispatcherQueue.tryTakeN(None).void
      _ <- userIdRef.set(newSettings.userId)
    yield ()
}

object ApplicationState {
  def make[F[_]: Temporal](settings: TestSettings): F[ApplicationState[F]] =
    for
      dispatcherQueue   <- Queue.bounded[F, Action](1024)
      clockRef          <- Ref.of[F, Option[Instant]](None)
      dataRef           <- Ref.of[F, Option[MarketTimeSeriesData]](None)
      marketStateRef    <- Ref.of[F, MarketState](settings.marketState)
      tradeSettingsRef  <- Ref.of[F, TradeSettings](settings.trade)
      tradeOrdersRef    <- Ref.of[F, ListBuffer[TradeOrderPlacement]](ListBuffer.empty)
      signalSettingsRef <- Ref.of[F, SignalSettings](settings.signal)
      userIdRef         <- Ref.of[F, UserId](settings.userId)
    yield ApplicationState[F](
      marketStateRef = marketStateRef,
      tradeSettingsRef = tradeSettingsRef,
      tradeOrdersRef = tradeOrdersRef,
      signalSettingsRef = signalSettingsRef,
      clockRef = clockRef,
      dataRef = dataRef,
      dispatcherQueue = dispatcherQueue,
      userIdRef = userIdRef
    )
}
