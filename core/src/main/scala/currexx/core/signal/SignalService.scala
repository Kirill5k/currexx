package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.calculations.MovingAverageCalculator
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.SignalRepository
import currexx.domain.market.{Condition, CurrencyPair, Indicator, MarketTimeSeriesData}
import fs2.Stream

import scala.util.Try

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]
  def getSettings(uid: UserId, pair: CurrencyPair): F[SignalSettings]
  def updateSettings(settings: SignalSettings): F[Unit]
  def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit]

final private class LiveSignalService[F[_]](
    private val repository: SignalRepository[F],
    private val dispatcher: ActionDispatcher[F]
)(using
    F: Concurrent[F]
) extends SignalService[F] {
  override def submit(signal: Signal): F[Unit] =
    repository.save(signal) >> dispatcher.dispatch(Action.SignalSubmitted(signal))

  override def getAll(uid: UserId): F[List[Signal]] =
    repository.getAll(uid)

  override def getSettings(uid: UserId, pair: CurrencyPair): F[SignalSettings] = ???
  override def updateSettings(settings: SignalSettings): F[Unit]               = ???

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    Stream(detectMacdCrossing(uid, data)).unNone.evalMap(submit).compile.drain

  private def detectMacdCrossing(uid: UserId, data: MarketTimeSeriesData): Option[Signal] = {
    val (macdLine, signalLine) = MovingAverageCalculator.macdWithSignal(data.prices.map(_.close).toList)
    Condition
      .lineCrossing(macdLine.head, signalLine.head, macdLine.drop(1).head, signalLine.drop(1).head)
      .map(c => Signal(uid, data.currencyPair, Indicator.MACD, c, data.prices.head.time))
  }
}

object SignalService:
  def make[F[_]: Concurrent](repository: SignalRepository[F], dispatcher: ActionDispatcher[F]): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](repository, dispatcher))
