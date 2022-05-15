package currexx.core.monitor.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.JsonCodecs
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import fs2.Stream
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

import scala.concurrent.duration.FiniteDuration

trait MonitorRepository[F[_]] extends Repository[F]:
  def stream: Stream[F, Monitor]
  def find(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Monitor]
  def getAll(uid: UserId): F[List[Monitor]]
  def create(monitor: CreateMonitor): F[MonitorId]
  def activate(uid: UserId, id: MonitorId, active: Boolean): F[Monitor]
  def update(mon: Monitor): F[Monitor]
  def updateQueriedTimestamp(uid: UserId, id: MonitorId): F[Monitor]

final private class LiveMonitorRepository[F[_]](
    private val collection: MongoCollection[F, MonitorEntity]
)(using
    F: Async[F]
) extends MonitorRepository[F] {

  override def stream: Stream[F, Monitor] =
    collection.find.stream.map(_.toDomain)

  override def find(uid: UserId, id: MonitorId): F[Monitor] =
    collection
      .find(idEq(id.value) && userIdEq(uid))
      .first
      .flatMap(maybeMon => F.fromOption(maybeMon.map(_.toDomain), AppError.EntityDoesNotExist("Monitor", id.value)))

  override def getAll(uid: UserId): F[List[Monitor]] =
    collection.find(userIdEq(uid)).all.map(_.map(_.toDomain).toList)

  override def create(mon: CreateMonitor): F[MonitorId] = {
    val entity = MonitorEntity.from(mon)
    collection
      .count(userIdAndCurrencyPairEq(mon.userId, mon.currencyPair))
      .flatMap {
        case 0 => collection.insertOne(entity).as(MonitorId(entity._id))
        case _ => AppError.AlreadyBeingMonitored(mon.currencyPair).raiseError[F, MonitorId]
      }
  }

  override def activate(uid: UserId, id: MonitorId, active: Boolean): F[Monitor] =
    runUpdate(uid, id)(Update.set(Field.Active, active))

  override def updateQueriedTimestamp(uid: UserId, id: MonitorId): F[Monitor] =
    runUpdate(uid, id)(Update.currentDate(Field.LastQueriedAt))

  private def runUpdate(uid: UserId, id: MonitorId)(update: Update): F[Monitor] =
    collection
      .findOneAndUpdate(idEq(id.value) && userIdEq(uid), update.currentDate(Field.LastUpdatedAt))
      .flatMap(maybeMon => F.fromOption(maybeMon.map(_.toDomain), AppError.EntityDoesNotExist("Monitor", id.value)))

  override def delete(uid: UserId, id: MonitorId): F[Monitor] =
    collection
      .findOneAndDelete(idEq(id.value) && userIdEq(uid))
      .flatMap(maybeMon => F.fromOption(maybeMon.map(_.toDomain), AppError.EntityDoesNotExist("Monitor", id.value)))

  override def update(mon: Monitor): F[Monitor] =
    collection
      .count(idEq(mon.id.value).not && userIdEq(mon.userId) && Filter.eq(Field.CurrencyPair, mon.currencyPair))
      .flatMap {
        case 0 =>
          runUpdate(mon.userId, mon.id) {
            Update
              .set(Field.Active, mon.active)
              .set(Field.CurrencyPair, mon.currencyPair)
              .set("interval", mon.interval)
              .set("period", mon.period)
              .set("schedule", mon.schedule)
          }
        case _ =>
          AppError.AlreadyBeingMonitored(mon.currencyPair).raiseError[F, Monitor]
      }
}

object MonitorRepository extends MongoJsonCodecs with JsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MonitorRepository[F]] =
    db.getCollectionWithCodec[MonitorEntity]("monitors")
      .map(_.withAddedCodec[CurrencyPair].withAddedCodec[Interval].withAddedCodec[FiniteDuration].withAddedCodec[Schedule])
      .map(coll => LiveMonitorRepository[F](coll))
