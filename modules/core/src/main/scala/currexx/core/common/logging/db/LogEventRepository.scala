package currexx.core.common.logging.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.logging.LogEvent
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.models.database.CreateCollectionOptions
import mongo4cats.database.MongoDatabase

trait LogEventRepository[F[_]] extends Repository[F]:
  def save(event: LogEvent): F[Unit]
  def getAll: F[List[LogEvent]]

final private class LiveLogEventRepository[F[_]](
    private val collection: MongoCollection[F, LogEventEntity]
)(using
    F: Async[F]
) extends LogEventRepository[F]:
  override def save(event: LogEvent): F[Unit] =
    collection.insertOne(LogEventEntity.from(event)).void

  override def getAll: F[List[LogEvent]] =
    collection.find.all.map(_.toList.map(_.toDomain))

object LogEventRepository extends MongoJsonCodecs:
  private val collectionName    = "log-events"
  private val collectionOptions = CreateCollectionOptions(capped = true, sizeInBytes = 268435456L)

  def make[F[_]](db: MongoDatabase[F])(using F: Async[F]): F[LogEventRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- F.unlessA(collNames.toSet.contains(collectionName))(db.createCollection(collectionName, collectionOptions))
      coll      <- db.getCollectionWithCodec[LogEventEntity](collectionName)
    yield LiveLogEventRepository[F](coll)
