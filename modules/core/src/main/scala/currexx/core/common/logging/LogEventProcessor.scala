package currexx.core.common.logging

import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.messenger.MessengerClient
import currexx.core.common.logging.db.LogEventRepository
import fs2.Stream
import mongo4cats.database.MongoDatabase
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait LogEventProcessor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveLogEventProcessor[F[_]: Async](
    private val repository: LogEventRepository[F],
    private val messenger: MessengerClient[F]
)(using
    logger: Logger[F]
) extends LogEventProcessor[F] {

  private val acceptedEvents = Set(LogLevel.Trace, LogLevel.Debug, LogLevel.Warn, LogLevel.Error)
  // Delivery failures must not enter the event queue and trigger more notifications.
  private val deliveryLogger = Slf4jLogger.getLogger[F]

  override def run: Stream[F, Unit] =
    logger.events
      .filter(e => acceptedEvents.contains(e.level))
      .evalMap { event =>
        repository
          .save(event)
          .flatMap { _ =>
            messenger
              .send(s"Currexx [${event.level.toString.toUpperCase}]", event.message)
              .handleErrorWith(error => deliveryLogger.error(error)("Failed to send log event notification"))
          }
      }
}

object LogEventProcessor:
  def make[F[_]: {Async, Logger}](database: MongoDatabase[F], messenger: MessengerClient[F]): F[LogEventProcessor[F]] =
    LogEventRepository.make(database).map(repo => LiveLogEventProcessor(repo, messenger))
