package currexx.core.common.action

import cats.Functor
import cats.effect.Concurrent
import cats.effect.std.Queue
import cats.syntax.functor.*
import fs2.Stream

trait ActionDispatcher[F[_]]:
  def dispatch(action: Action): F[Unit]
  def actions: Stream[F, Action]

final private class LiveActionDispatcher[F[_]: Functor](
    private val submittedActions: Queue[F, Action]
) extends ActionDispatcher[F] {

  override def dispatch(action: Action): F[Unit] =
    submittedActions.offer(action)

  override def actions: Stream[F, Action] =
    Stream.fromQueueUnterminated(submittedActions)
}

object ActionDispatcher:
  def make[F[_]: Concurrent]: F[ActionDispatcher[F]] =
    Queue.bounded[F, Action](1024).map(LiveActionDispatcher[F](_))
