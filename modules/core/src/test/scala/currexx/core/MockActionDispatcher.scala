package currexx.core

import cats.Monad
import cats.syntax.functor.*
import currexx.core.common.action.{Action, ActionDispatcher}
import fs2.Stream

import scala.collection.mutable.ListBuffer

final private class MockActionDispatcher[F[_]](
    val submittedActions: ListBuffer[Action]
)(using
    F: Monad[F]
) extends ActionDispatcher[F]:

  override def dispatch(action: Action): F[Unit] =
    F.unit.map(_ => submittedActions.addOne(action)).void

  override def actions: fs2.Stream[F, Action] =
    Stream.emits(submittedActions)

object MockActionDispatcher:
  def apply[F[_]: Monad] = make[F]
  def make[F[_]: Monad]  = new MockActionDispatcher[F](ListBuffer.empty)
