package currexx.core.common.logging.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.logging.LogEvent
import mongo4cats.collection.MongoCollection
import mongo4cats.database.{CreateCollectionOptions, MongoDatabase}

trait LogEventRepository[F[_]] extends Repository[F]:
  def save(event: LogEvent): F[Unit]

final private class LiveLogEventRepository[F[_]](
    private val collection: MongoCollection[F, LogEventEntity]
)(using
    F: Async[F]
) extends LogEventRepository[F]:
  override def save(event: LogEvent): F[Unit] =
    collection.insertOne(LogEventEntity.from(event)).void

object LogEventRepository:
  private val collectionName    = "log-events"
  private val collectionOptions = CreateCollectionOptions().capped(true).sizeInBytes(268435456L)

  def make[F[_]: Async](db: MongoDatabase[F]): F[LogEventRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- if (collNames.toSet.contains(collectionName)) ().pure[F] else db.createCollection(collectionName, collectionOptions)
      coll      <- db.getCollectionWithCodec[LogEventEntity](collectionName)
    yield LiveSignalRepository[F](coll)
