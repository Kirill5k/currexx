package currexx.core.settings.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.settings.Settings
import currexx.domain.user.UserId
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Aggregate, Projection}
import mongo4cats.database.MongoDatabase

trait SettingsRepository[F[_]]:
  def get(uid: UserId): F[Option[Settings]]
  def createFor(uid: UserId): F[Unit]

final private class LiveSettingsRepository[F[_]](
    private val collection: MongoCollection[F, SettingsEntity]
)(using
    F: Async[F]
) extends SettingsRepository[F] with Repository[F] {

  override def get(uid: UserId): F[Option[Settings]] =
    collection
      .aggregate[SettingsEntity] {
        Aggregate
          .matchBy(userIdEq(uid))
          .lookup("signal-settings", Field.UId, Field.UId, "signals")
          .lookup("trade-settings", Field.UId, Field.UId, "trades")
          .project(
            Projection
              .include(Field.UId)
              .computed("trade", Document("$first" := "$trades"))
              .computed("signal", Document("$first" := "$signals"))
          )
      }
      .first
      .mapOption(_.toDomain)

  override def createFor(uid: UserId): F[Unit] =
    collection
      .count(userIdEq(uid))
      .flatMap {
        case 0 => collection.insertOne(SettingsEntity.from(uid)).void
        case _ => F.unit
      }
}

object SettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[SettingsRepository[F]] =
    db.getCollectionWithCodec[SettingsEntity]("settings")
      .map(coll => LiveSettingsRepository[F](coll))
