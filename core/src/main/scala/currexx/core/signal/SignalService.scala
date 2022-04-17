package currexx.core.signal

import cats.Monad
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.option.*
import currexx.domain.user.UserId
import currexx.calculations.MovingAverageCalculator
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.SignalRepository
import currexx.domain.errors.AppError
import currexx.domain.market.{Condition, Indicator, MarketTimeSeriesData}

import scala.util.Try

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]
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

  override def processMarketData(uid: UserId, data: MarketTimeSeriesData): F[Unit] =
    F.fromEither(detectMacdCrossing(uid, data)).flatMap {
      case Some(signal) => submit(signal)
      case None         => ().pure[F]
    }

  private def detectMacdCrossing(uid: UserId, data: MarketTimeSeriesData): Either[AppError, Option[Signal]] =
    Try {
      val closingPrices                = data.prices.map(_.close)
      val (macdLine, signalLine)       = MovingAverageCalculator.macdWithSignal(closingPrices.toList)
      val List(macdCurr, macdPrev)     = macdLine.take(2)
      val List(signalCurr, signalPrev) = signalLine.take(2)
      Condition
        .lineCrossing(macdCurr, signalCurr, macdPrev, signalPrev)
        .map(c => Signal(uid, data.currencyPair, Indicator.MACD, c, data.prices.head.time))
    }.toEither
      .leftMap(e => AppError.FailedCalculation(s"Error during macd crossing detection for ${data.currencyPair}: ${e.getMessage}"))
}

object SignalService:
  def make[F[_]: Concurrent](repository: SignalRepository[F], dispatcher: ActionDispatcher[F]): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](repository, dispatcher))
