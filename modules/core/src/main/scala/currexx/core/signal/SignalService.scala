package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.common.http.SearchParams
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData}
import currexx.domain.signal.Indicator
import kirill5k.common.cats.Clock
import kirill5k.common.syntax.time.*

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId, sp: SearchParams): F[List[Signal]]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData, detector: SignalDetector = SignalDetector.pure): F[Unit]

final private class LiveSignalService[F[_]](
    private val signalRepo: SignalRepository[F],
    private val settingsRepo: SignalSettingsRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F],
    clock: Clock[F]
) extends SignalService[F] {
  override def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] = signalRepo.getAll(uid, sp)
  override def submit(signal: Signal): F[Unit] = saveAndDispatchAction(signal.userId, signal.currencyPair, List(signal))

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData, detector: SignalDetector): F[Unit] =
    clock.now
      .map(_.durationBetween(data.prices.head.time))
      .flatMap { timeGap =>
        F.whenA(timeGap < data.interval.toDuration * 2) {
          for
            settings <- settingsRepo.get(uid)
            signals = settings.indicators.flatMap(detector.detect(uid, data))
            _ <- F.whenA(signals.nonEmpty)(saveAndDispatchAction(uid, data.currencyPair, signals))
          yield ()
        }
      }

  private def saveAndDispatchAction(uid: UserId, cp: CurrencyPair, signals: List[Signal]) =
    signalRepo.saveAll(signals) >>
      dispatcher.dispatch(Action.ProcessSignals(uid, cp, signals))
}

object SignalService:
  def make[F[_]: {Concurrent, Clock}](
      signalRepo: SignalRepository[F],
      settingsRepo: SignalSettingsRepository[F],
      dispatcher: ActionDispatcher[F]
  ): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](signalRepo, settingsRepo, dispatcher))
