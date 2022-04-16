package currexx.core.monitor.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.errors.AppError.EntityDoesNotExist
import currexx.domain.user.UserId
import fs2.Stream
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

trait MonitorRepository[F[_]] extends Repository[F]:
  def find(uid: UserId, id: MonitorId): F[Monitor]
  def getAll(uid: UserId): F[List[Monitor]]
  def stream(uid: UserId): Stream[F, Monitor]
  def create(monitor: CreateMonitor): F[MonitorId]
  def activate(uid: UserId, id: MonitorId, active: Boolean): F[Unit]

final private class LiveMonitorRepository[F[_]](
    private val collection: MongoCollection[F, MonitorEntity]
)(using
    F: Async[F]
) extends MonitorRepository[F] {

  override def find(uid: UserId, id: MonitorId): F[Monitor] =
    collection
      .find(idEq(id.value) && userIdEq(uid))
      .first
      .flatMap(maybeMon => F.fromOption(maybeMon.map(_.toDomain), EntityDoesNotExist("Monitor", id.value)))

  override def getAll(uid: UserId): F[List[Monitor]] =
    collection.find(userIdEq(uid)).all.map(_.map(_.toDomain).toList)

  override def stream(uid: UserId): Stream[F, Monitor] =
    collection.find(userIdEq(uid)).stream.map(_.toDomain)

  override def create(monitor: CreateMonitor): F[MonitorId] = {
    val entity = MonitorEntity.from(monitor)
    collection.insertOne(entity).as(MonitorId(entity._id))
  }

  def activate(uid: UserId, id: MonitorId, active: Boolean): F[Unit] =
    collection
      .updateOne(idEq(id.value) && userIdEq(uid), Update.set("active", active))
      .map(_.getMatchedCount)
      .flatMap(notFoundErrorIfNoMatches(id))

  private def notFoundErrorIfNoMatches(id: MonitorId)(matchCount: Long): F[Unit] =
    if (matchCount == 0) EntityDoesNotExist("Monitor", id.value).raiseError[F, Unit] else ().pure[F]
}

object MonitorRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MonitorRepository[F]] =
    db.getCollectionWithCodec[MonitorEntity]("monitors")
      .map(coll => LiveMonitorRepository[F](coll))
