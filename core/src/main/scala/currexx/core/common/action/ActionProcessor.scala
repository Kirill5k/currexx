package currexx.core.common.action

import cats.Monad
import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import currexx.core.common.errors.AppError
import fs2.Stream
import org.typelevel.log4cats.Logger

import scala.concurrent.duration.*

trait ActionProcessor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveActionProcessor[F[_]: Temporal](
    private val dispatcher: ActionDispatcher[F]
)(using
    logger: Logger[F]
) extends ActionProcessor[F] {

  override def run: Stream[F, Unit] =
    dispatcher.stream.evalMap(handleAction)

  private def handleAction(action: Action): F[Unit] =
    (action match {
      case Action.SignalSubmitted(signal) => logger.info(s"received signal submitted action $signal")
    }).handleErrorWith {
      case error: AppError =>
        logger.warn(error)(s"domain error while processing action $action")
      case error =>
        logger.error(error)(s"unexpected error processing action $action") *>
          Temporal[F].sleep(1.second) *>
          dispatcher.dispatch(action)
    }
}

object ActionProcessor:
  def make[F[_]: Temporal: Logger](dispatcher: ActionDispatcher[F]): F[ActionProcessor[F]] =
    Monad[F].pure(new LiveActionProcessor[F](dispatcher))
