package currexx.core.monitor.db

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.{Limits, Schedule}
import currexx.domain.user.UserId
import fs2.Stream
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

import scala.concurrent.duration.FiniteDuration

trait MonitorRepository[F[_]] extends Repository[F]:
  def stream: Stream[F, Monitor]
  def find(uid: UserId, id: MonitorId): F[Monitor]
  def delete(uid: UserId, id: MonitorId): F[Monitor]
  def getAll(uid: UserId): F[List[Monitor]]
  def create(monitor: CreateMonitor): F[Monitor]
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
      .mapOption(_.toDomain)
      .flatMap(maybeMon => F.fromOption(maybeMon, AppError.EntityDoesNotExist("Monitor", id.value)))

  override def getAll(uid: UserId): F[List[Monitor]] =
    collection.find(userIdEq(uid)).all.mapIterable(_.toDomain)

  override def create(mon: CreateMonitor): F[Monitor] =
    val cps = mon.currencyPairs.toList
    collection
      .count(uidAndKindAndCurrencyPairs(mon.userId, mon.kind, cps))
      .flatMap {
        case 0 =>
          val entity = MonitorEntity.from(mon)
          collection.insertOne(entity).as(entity.toDomain)
        case _ =>
          alreadyBeingMonitoredError(mon.userId, mon.kind, cps)
      }

  override def activate(uid: UserId, id: MonitorId, active: Boolean): F[Monitor] =
    runUpdate(uid, id)(Update.set(Field.Active, active))

  override def updateQueriedTimestamp(uid: UserId, id: MonitorId): F[Monitor] =
    runUpdate(uid, id)(Update.currentDate(Field.LastQueriedAt))

  private def runUpdate(uid: UserId, id: MonitorId)(update: Update): F[Monitor] =
    collection
      .findOneAndUpdate(idEq(id.value) && userIdEq(uid), update.currentDate(Field.LastUpdatedAt))
      .mapOption(_.toDomain)
      .flatMap(maybeMon => F.fromOption(maybeMon, AppError.EntityDoesNotExist("Monitor", id.value)))

  override def delete(uid: UserId, id: MonitorId): F[Monitor] =
    collection
      .findOneAndDelete(idEq(id.value) && userIdEq(uid))
      .mapOption(_.toDomain)
      .flatMap(maybeMon => F.fromOption(maybeMon, AppError.EntityDoesNotExist("Monitor", id.value)))

  override def update(mon: Monitor): F[Monitor] =
    val baseUpdate = Update
      .set(Field.Active, mon.active)
      .set(Field.CurrencyPairs, mon.currencyPairs.toList)
      .set("schedule", mon.schedule)
      .set(Field.LastQueriedAt, mon.lastQueriedAt)
    val cps = mon.currencyPairs.toList
    collection
      .count(idEq(mon.id.value).not && uidAndKindAndCurrencyPairs(mon.userId, mon.kind, cps))
      .flatMap {
        case 0 =>
          runUpdate(mon.userId, mon.id) {
            mon match
              case md: Monitor.MarketData => baseUpdate.set("interval", md.interval)
              case p: Monitor.Profit      => baseUpdate.set("limits", p.limits)
          }
        case _ => alreadyBeingMonitoredError(mon.userId, mon.kind, cps)
      }

  private def alreadyBeingMonitoredError(uid: UserId, kind: String, cps: List[CurrencyPair]): F[Monitor] =
    collection
      .distinct[CurrencyPair](Field.CurrencyPairs, uidAndKindAndCurrencyPairs(uid, kind, cps))
      .all
      .flatMap { alreadyTracked =>
        F.raiseError(AppError.AlreadyBeingMonitored(alreadyTracked.toSet.intersect(cps.toSet)))
      }

  private def uidAndKindAndCurrencyPairs(uid: UserId, kind: String, cps: List[CurrencyPair]): Filter =
    userIdEq(uid) && Filter.eq(Field.Kind, kind) && Filter.in(Field.CurrencyPairs, cps)
}

object MonitorRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MonitorRepository[F]] =
    db.getCollectionWithCodec[MonitorEntity]("monitors")
      .map(_.withAddedCodec[CurrencyPair].withAddedCodec[Interval].withAddedCodec[Schedule].withAddedCodec[Limits])
      .map(coll => LiveMonitorRepository[F](coll))
