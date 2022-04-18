package currexx.domain

import cats.Monad
import cats.effect.kernel.Temporal
import cats.effect.std.Queue
import cats.effect.{Async, Deferred}
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger as Logger4Cats

import java.time.Instant

object logging {

  final case class LoggedError(
      message: String,
      time: Instant
  )

  trait Logger[F[_]] extends Logger4Cats[F] {
    def errors: Stream[F, LoggedError]
  }

  final private class LiveLogger[F[_]](
      private val logger: Logger4Cats[F],
      private val loggedErrors: Queue[F, LoggedError]
  )(using
    F: Temporal[F]
  ) extends Logger[F] {

    private def enqueue(t: Throwable, message: => String): F[Unit] =
      enqueue(s"${message.split("\n").head} - ${t.getMessage}")

    private def enqueue(message: => String): F[Unit] =
      F.realTimeInstant.flatMap(t => loggedErrors.offer(LoggedError(message.split("\n").head, t)))

    override def errors: Stream[F, LoggedError] =
      Stream.fromQueueUnterminated(loggedErrors)

    override def error(t: Throwable)(message: => String): F[Unit] =
      enqueue(t, message) *> logger.error(t)(message)

    override def error(message: => String): F[Unit] =
      enqueue(message) *> logger.error(message)

    override def warn(t: Throwable)(message: => String): F[Unit] =
      logger.warn(t)(message)

    override def info(t: Throwable)(message: => String): F[Unit] =
      logger.info(t)(message)

    override def debug(t: Throwable)(message: => String): F[Unit] =
      logger.debug(t)(message)

    override def trace(t: Throwable)(message: => String): F[Unit] =
      logger.trace(t)(message)

    override def warn(message: => String): F[Unit] =
      logger.warn(message)

    override def info(message: => String): F[Unit] =
      logger.info(message)

    override def debug(message: => String): F[Unit] =
      logger.debug(message)

    override def trace(message: => String): F[Unit] =
      logger.trace(message)
  }

  object Logger {
    def apply[F[_]](using ev: Logger[F]): Logger[F] = ev

    def make[F[_]: Async]: F[Logger[F]] =
      Queue.unbounded[F, LoggedError].map(q => LiveLogger[F](Slf4jLogger.getLogger[F], q))
  }
}
