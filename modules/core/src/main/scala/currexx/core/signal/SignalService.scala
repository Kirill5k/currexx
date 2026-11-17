package currexx.core.signal

import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.settings.StrategyRuntime
import currexx.core.signal.db.SignalRepository
import currexx.domain.errors.AppError
import currexx.domain.market.MarketTimeSeriesData
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger

trait SignalService[F[_]]:
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData, detector: SignalDetector = SignalDetector.pure): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val runtime: StrategyRuntime[F],
    private val dispatcher: ActionDispatcher[F]
)(using F: Concurrent[F], clock: Clock[F], logger: Logger[F])
    extends SignalService[F]:
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] = signalRepo.getAll(uid, sp)

  def processMarketData(uid: UserId, data: MarketTimeSeriesData, detector: SignalDetector): F[Unit] =
    for
      config   <- runtime.load(uid)
      settings <- F.fromOption(config.signal, AppError.NotSetup("Signal"))
      gap      <- clock.durationBetweenNowAnd(data.latestTime)
      fresh   = gap < data.interval.toDuration * 2
      signals = if fresh then settings.indicators.flatMap(detector.detect(uid, data)) else Nil
      _ <- F.unlessA(fresh)(logger.info(s"skipping signal detection for stale candle ${data.latestTime}"))
      batch = SignalBatch(
        interval = data.interval,
        revision = config.revision,
        candleTime = data.latestTime,
        closureGap = SignalBatch.closureGap(data),
        sources = config.sources,
        signals = signals
      )
      _ <- F.whenA(signals.nonEmpty)(signalRepo.saveAll(signals))
      _ <- dispatcher.dispatch(Action.ProcessSignals(uid, data.currencyPair, batch))
    yield ()

object SignalService:
  def make[F[_]: {Concurrent, Clock, Logger}](
      signalRepo: SignalRepository[F],
      runtime: StrategyRuntime[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    LiveSignalService(signalRepo, runtime, dispatcher).pure[F]
