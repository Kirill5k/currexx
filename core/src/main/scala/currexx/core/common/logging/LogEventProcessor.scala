package currexx.core.common.logging

import currexx.core.common.logging.db.LogEventRepository
import fs2.Stream

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

