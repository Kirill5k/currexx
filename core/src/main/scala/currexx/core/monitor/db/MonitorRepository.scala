package currexx.core.monitor.db

import cats.effect.Async
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import fs2.Stream
import mongo4cats.collection.MongoCollection

trait MonitorRepository[F[_]] extends Repository[F]:
  def get(userId: UserId, id: MonitorId): F[Option[Monitor]]
  def getAll(userId: UserId): F[List[Monitor]]
  def stream(userId: UserId): Stream[F, Monitor]
  def create(create: CreateMonitor): F[MonitorId]

final private class LiveMonitorRepository[F[_]: Async](
    private val collection: MongoCollection[F, MonitorEntity]
) extends MonitorRepository[F] {

  override def get(userId: Any, id: MonitorId): F[Option[Monitor]] = ???

  override def getAll(userId: Any): F[List[Monitor]] = ???

  override def stream(userId: Any): Stream[F, Monitor] = ???

  override def create(create: CreateMonitor): F[MonitorId] = {
    val monitor = MonitorEntity.from(create)
    collection.insertOne(monitor).as(monitor.id)
  }
}
