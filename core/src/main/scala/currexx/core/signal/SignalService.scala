package currexx.core.signal

import cats.Monad
import cats.syntax.flatMap.*
import currexx.domain.user.UserId
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.signal.db.SignalRepository

trait SignalService[F[_]]:
  def submit(signal: Signal): F[Unit]
  def getAll(uid: UserId): F[List[Signal]]

final private class LiveSignalService[F[_]: Monad](
    private val repository: SignalRepository[F],
    private val dispatcher: ActionDispatcher[F]
) extends SignalService[F]:
  override def submit(signal: Signal): F[Unit] =
    repository.save(signal) >> dispatcher.dispatch(Action.SignalSubmitted(signal))

  override def getAll(uid: UserId): F[List[Signal]] =
    repository.getAll(uid)

object SignalService:
  def make[F[_]: Monad](repository: SignalRepository[F], dispatcher: ActionDispatcher[F]): F[SignalService[F]] =
    Monad[F].pure(LiveSignalService[F](repository, dispatcher))
