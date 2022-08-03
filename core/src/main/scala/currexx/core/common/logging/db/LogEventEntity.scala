package currexx.core.common.logging.db

import currexx.core.common.logging.{LogEvent, LogLevel}
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class LogEventEntity(
    _id: ObjectId,
    level: String,
    time: Instant,
    message: String
) derives Codec.AsObject:
  def toDomain: LogEvent = LogEvent(LogLevel.valueOf(level.toLowerCase.capitalize), time, message)

object LogEventEntity:
  def from(event: LogEvent): LogEventEntity =
    LogEventEntity(ObjectId.get, event.level.toString.toUpperCase, event.time, event.message)
