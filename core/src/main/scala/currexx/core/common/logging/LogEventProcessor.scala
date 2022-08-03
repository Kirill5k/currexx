package currexx.core.common.logging

import cats.effect.Async
import cats.syntax.functor.*
import currexx.core.common.logging.db.LogEventRepository
import fs2.Stream
import mongo4cats.database.MongoDatabase

trait LogEventProcessor[F[_]]:
  def run: Stream[F, Unit]

final private class LiveLogEventProcessor[F[_]](
    private val repository: LogEventRepository[F]
)(using
    logger: Logger[F]
) extends LogEventProcessor[F] {

  override def run: Stream[F, Unit] =
    logger.events.evalMap(repository.save)
}

object LogEventProcessor:
  def make[F[_] : Async: Logger](database: MongoDatabase[F]): F[LogEventProcessor[F]] =
    LogEventRepository.make(database).map(repo => LiveLogEventProcessor(repo))