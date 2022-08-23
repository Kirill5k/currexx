package currexx.core.settings.db

import cats.effect.Async
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.settings.Settings
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Aggregate, Filter, Projection}
import mongo4cats.database.MongoDatabase

trait SettingsRepository[F[_]]:
  def get(uid: UserId): F[Option[Settings]]

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
          .lookup("signal-settings", Field.UId, Field.UId, "signal")
          .lookup("trade-settings", Field.UId, Field.UId, "trade")
      }
      .first
      .map(_.map(_.toDomain))
}

object SettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[SettingsRepository[F]] =
    db.getCollectionWithCodec[SettingsEntity]("settings")
      .map(coll => LiveSettingsRepository[F](coll))
