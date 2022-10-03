package currexx.core

import cats.Monad
import currexx.core.common.action.{Action, ActionDispatcher}
import fs2.Stream

import scala.collection.mutable.ListBuffer

final private class MockActionDispatcher[F[_]](
    val submittedActions: ListBuffer[Action]
)(using
    F: Monad[F]
) extends ActionDispatcher[F]:

  override def dispatch(action: Action): F[Unit] = {
    submittedActions.addOne(action)
    F.unit
  }

  override def pendingActions: F[List[Action]] = F.pure(submittedActions.toList)

  override def actions: fs2.Stream[F, Action] =
    Stream.emits(submittedActions)

object MockActionDispatcher:
  def make[F[_]: Monad] = new MockActionDispatcher[F](ListBuffer.empty)
