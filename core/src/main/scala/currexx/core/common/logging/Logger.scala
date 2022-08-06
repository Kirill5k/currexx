package currexx.core.common.logging

import cats.effect.{Async, Temporal}
import cats.effect.std.Queue
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import fs2.Stream
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.typelevel.log4cats.Logger as Logger4Cats

import java.time.Instant

private[logging] enum LogLevel:
  case Trace, Debug, Info, Warn, Error

final private[logging] case class LogEvent(level: LogLevel, time: Instant, message: String)

trait Logger[F[_]] extends Logger4Cats[F] {
  def events: Stream[F, LogEvent]
}

final private class LiveLogger[F[_]](
    private val logger: Logger4Cats[F],
    private val loggedEvents: Queue[F, LogEvent]
)(using
    F: Temporal[F]
) extends Logger[F] {

  private def enqueue(level: LogLevel, t: Throwable, message: => String): F[Unit] =
    enqueue(level, s"${message.split("\n").head} - ${t.getMessage}")

  private def enqueue(level: LogLevel, message: => String): F[Unit] =
    F.realTimeInstant.flatMap(t => loggedEvents.offer(LogEvent(level, t, message.split("\n").head)))

  override def events: Stream[F, LogEvent]                      = Stream.fromQueueUnterminated(loggedEvents)
  override def error(t: Throwable)(message: => String): F[Unit] = enqueue(LogLevel.Error, t, message) *> logger.error(t)(message)
  override def error(message: => String): F[Unit]               = enqueue(LogLevel.Error, message) *> logger.error(message)
  override def warn(t: Throwable)(message: => String): F[Unit]  = enqueue(LogLevel.Warn, t, message) *> logger.warn(t)(message)
  override def warn(message: => String): F[Unit]                = enqueue(LogLevel.Warn, message) *> logger.warn(message)
  override def debug(t: Throwable)(message: => String): F[Unit] = enqueue(LogLevel.Debug, t, message) *> logger.debug(t)(message)
  override def debug(message: => String): F[Unit]               = enqueue(LogLevel.Debug, message) *> logger.debug(message)
  override def trace(message: => String): F[Unit]               = enqueue(LogLevel.Trace, message) *> logger.trace(message)
  override def trace(t: Throwable)(message: => String): F[Unit] = enqueue(LogLevel.Trace, t, message) *> logger.trace(t)(message)
  override def info(message: => String): F[Unit]                = enqueue(LogLevel.Info, message) *> logger.info(message)
  override def info(t: Throwable)(message: => String): F[Unit]  = enqueue(LogLevel.Info, t, message) *> logger.info(t)(message)
}

object Logger {
  def apply[F[_]](using ev: Logger[F]): Logger[F] = ev

  def make[F[_]: Async]: F[Logger[F]] =
    Queue.unbounded[F, LogEvent].map(q => LiveLogger[F](Slf4jLogger.getLogger[F], q))
}
