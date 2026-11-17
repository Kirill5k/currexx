package currexx.backtest.services

import cats.MonadThrow
import cats.data.NonEmptyList
import cats.effect.{Ref, Temporal}
import cats.effect.std.Queue
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.backtest.{DataWindow, MarketMark, TestSettings}
import currexx.core.common.action.Action
import currexx.core.market.MarketState
import currexx.core.settings.{GlobalSettings, StrategyConfiguration, TradeSettings}
import currexx.core.trade.TradeOrderPlacement
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData, OpenedTradeOrder}
import currexx.domain.user.UserId
import kirill5k.common.syntax.time.*

import java.time.Instant
import scala.collection.mutable.ListBuffer
import scala.concurrent.duration.*

final class ApplicationState[F[_]](
    val marketStateRef: Ref[F, MarketState],
    val configurationRef: Ref[F, StrategyConfiguration],
    val tradeSettingsRef: Ref[F, TradeSettings],
    val tradeOrdersRef: Ref[F, ListBuffer[TradeOrderPlacement]],
    val clockRef: Ref[F, Option[Instant]],
    val dataRef: Ref[F, Option[MarketTimeSeriesData]],
    val marketMarksRef: Ref[F, List[MarketMark]],
    val dataWindowRef: Ref[F, Option[DataWindow]],
    val dispatcherQueue: Queue[F, Action],
    val userIdRef: Ref[F, UserId],
    val brokerPositionsRef: Ref[F, Map[CurrencyPair, OpenedTradeOrder]]
)(using F: MonadThrow[F]) {

  private val fetchTimeOffset: FiniteDuration = 100.seconds

  // The simulator clock includes a fetch delay; accounting uses the open at which the order actually filled.
  def accountingTime(time: Instant): Instant = time.minusNanos(fetchTimeOffset.toNanos)

  // Stages the current bar for execution: orders fill at the bar's open (close overwritten with open),
  // records the real close for equity accounting, and advances the clock to the
  // fetch time. Signals still run against the fully closed previous candle, so fills never use a close
  // that is only known retrospectively.
  def prepareExecution(currentData: MarketTimeSeriesData): F[Unit] =
    val currentBar    = currentData.prices.head
    val executionBar  = currentBar.copy(close = currentBar.open)
    val executionData = currentData.copy(prices = NonEmptyList(executionBar, currentData.prices.tail))
    val executionTime = currentBar.time.plus(fetchTimeOffset)
    val closeMark     = MarketMark(
      price = BigDecimal(currentBar.close),
      // A bar's end is exclusive: a close at midnight belongs to the preceding day/month.
      observedAt = currentBar.time.plus(currentData.interval.toDuration).minusNanos(1)
    )
    val barWindow = DataWindow(currentBar.time, closeMark.observedAt)
    for
      _ <- marketMarksRef.update(closeMark :: _)
      _ <- dataRef.set(Some(executionData))
      _ <- clockRef.set(Some(executionTime))
      _ <- dataWindowRef.update(_.fold(Some(barWindow))(window => Some(window.union(barWindow))))
    yield ()

  def reset(newSettings: TestSettings): F[Unit] =
    for
      configuration <- F.fromEither(
        StrategyConfiguration.from(GlobalSettings(newSettings.userId, Some(newSettings.signal), Some(newSettings.trade), None))
      )
      _ <- configurationRef.update(old => configuration.copy(revision = old.revision + 1))
      _ <- marketStateRef.set(newSettings.marketState)
      _ <- tradeSettingsRef.set(newSettings.trade)
      _ <- tradeOrdersRef.set(ListBuffer.empty[TradeOrderPlacement])
      _ <- clockRef.set(None)
      _ <- dataRef.set(None)
      _ <- marketMarksRef.set(Nil)
      _ <- dataWindowRef.set(None)
      _ <- dispatcherQueue.tryTakeN(None).void
      _ <- userIdRef.set(newSettings.userId)
      _ <- brokerPositionsRef.set(Map.empty)
    yield ()
}

object ApplicationState {
  def make[F[_]: Temporal](settings: TestSettings): F[ApplicationState[F]] =
    for
      configuration <- Temporal[F].fromEither(
        StrategyConfiguration.from(GlobalSettings(settings.userId, Some(settings.signal), Some(settings.trade), None))
      )
      configurationRef <- Ref.of[F, StrategyConfiguration](configuration)
      dispatcherQueue  <- Queue.bounded[F, Action](1024)
      clockRef         <- Ref.of[F, Option[Instant]](None)
      dataRef          <- Ref.of[F, Option[MarketTimeSeriesData]](None)
      marketMarksRef   <- Ref.of[F, List[MarketMark]](Nil)
      dataWindowRef    <- Ref.of[F, Option[DataWindow]](None)
      marketStateRef   <- Ref.of[F, MarketState](settings.marketState)
      tradeSettingsRef <- Ref.of[F, TradeSettings](settings.trade)
      tradeOrdersRef   <- Ref.of[F, ListBuffer[TradeOrderPlacement]](ListBuffer.empty)
      userIdRef        <- Ref.of[F, UserId](settings.userId)
      brokerPositions  <- Ref.of[F, Map[CurrencyPair, OpenedTradeOrder]](Map.empty)
    yield ApplicationState[F](
      marketStateRef = marketStateRef,
      configurationRef = configurationRef,
      tradeSettingsRef = tradeSettingsRef,
      tradeOrdersRef = tradeOrdersRef,
      clockRef = clockRef,
      dataRef = dataRef,
      marketMarksRef = marketMarksRef,
      dataWindowRef = dataWindowRef,
      dispatcherQueue = dispatcherQueue,
      userIdRef = userIdRef,
      brokerPositionsRef = brokerPositions
    )
}
