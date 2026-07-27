package currexx.backtest.services

import cats.Monad
import cats.data.NonEmptyList
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.backtest.{MarketMark, TestSettings}
import currexx.core.common.action.Action
import currexx.core.market.MarketState
import currexx.core.settings.{SignalSettings, TradeSettings}
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

final class ApplicationState[F[_]](
    val marketStateRef: Ref[F, MarketState],
    val tradeSettingsRef: Ref[F, TradeSettings],
    val tradeOrdersRef: Ref[F, ListBuffer[TradeOrderPlacement]],
    val signalSettingsRef: Ref[F, SignalSettings],
    val clockRef: Ref[F, Option[Instant]],
    val dataRef: Ref[F, Option[MarketTimeSeriesData]],
    val finalMarkRef: Ref[F, Option[MarketMark]],
    val dispatcherQueue: Queue[F, Action],
    val userIdRef: Ref[F, UserId]
)(using F: Monad[F]) {

  private val fetchTimeOffset: FiniteDuration = 100.seconds

  // Stages the current bar for execution: orders fill at the bar's open (close overwritten with open),
  // records the mark-to-market of the real close for final accounting, and advances the clock to the
  // fetch time. Signals still run against the fully closed previous candle, so fills never use a close
  // that is only known retrospectively.
  def prepareExecution(currentData: MarketTimeSeriesData): F[Unit] =
    val currentBar    = currentData.prices.head
    val executionBar  = currentBar.copy(close = currentBar.open)
    val executionData = currentData.copy(prices = NonEmptyList(executionBar, currentData.prices.tail))
    val executionTime = currentBar.time.plus(fetchTimeOffset)
    val finalMark     = MarketMark(
      price = BigDecimal(currentBar.close),
      observedAt = executionTime.plus(currentData.interval.toDuration)
    )
    for
      _ <- finalMarkRef.set(Some(finalMark))
      _ <- dataRef.set(Some(executionData))
      _ <- clockRef.set(Some(executionTime))
    yield ()

  def reset(newSettings: TestSettings): F[Unit] =
    for
      _ <- marketStateRef.set(newSettings.marketState)
      _ <- tradeSettingsRef.set(newSettings.trade)
      _ <- tradeOrdersRef.set(ListBuffer.empty[TradeOrderPlacement])
      _ <- signalSettingsRef.set(newSettings.signal)
      _ <- clockRef.set(None)
      _ <- dataRef.set(None)
      _ <- finalMarkRef.set(None)
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
      finalMarkRef      <- Ref.of[F, Option[MarketMark]](None)
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
      finalMarkRef = finalMarkRef,
      dispatcherQueue = dispatcherQueue,
      userIdRef = userIdRef
    )
}
