package currexx.core.settings.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.settings.{GlobalSettings, SignalSettings, TradeSettings}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.Update
import mongo4cats.database.MongoDatabase

trait SettingsRepository[F[_]]:
  def get(uid: UserId): F[GlobalSettings]
  def update(gs: GlobalSettings): F[Unit]
  def createFor(uid: UserId): F[Unit]

final private class LiveSettingsRepository[F[_]](
    private val collection: MongoCollection[F, GlobalSettingsEntity]
)(using
    F: Async[F]
) extends SettingsRepository[F] with Repository[F] {

  override def update(gs: GlobalSettings): F[Unit] =
    collection
      .count(userIdEq(gs.userId))
      .flatMap {
        case 0 =>
          collection.insertOne(GlobalSettingsEntity.from(gs.userId, gs.signal, gs.trade, gs.note)).void
        case _ =>
          collection.updateOne(userIdEq(gs.userId), Update.set("signal", gs.signal).set("trade", gs.trade).set("note", gs.note)).void
      }

  override def get(uid: UserId): F[GlobalSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap {
        case Some(settings) => F.pure(settings.toDomain)
        case None           => F.raiseError(AppError.NotSetup("Global"))
      }

  override def createFor(uid: UserId): F[Unit] =
    collection
      .count(userIdEq(uid))
      .flatMap {
        case 0 => collection.insertOne(GlobalSettingsEntity.from(uid)).void
        case _ => F.unit
      }
}

object SettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[SettingsRepository[F]] =
    db.getCollectionWithCodec[GlobalSettingsEntity]("settings")
      .map(_.withAddedCodec[SignalSettings].withAddedCodec[TradeSettings])
      .map(LiveSettingsRepository[F](_))
