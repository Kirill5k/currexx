package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.settings.SignalSettings
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import mongo4cats.bson.Document
import mongo4cats.circe.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase

trait SignalSettingsRepository[F[_]] extends Repository[F]:
  def get(uid: UserId): F[SignalSettings]

final private class LiveSignalSettingsRepository[F[_]](
    private val collection: MongoCollection[F, Document]
)(using
    F: Async[F]
) extends SignalSettingsRepository[F]:
  override def get(uid: UserId): F[SignalSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(gs => F.fromOption(gs, AppError.NotSetup("Global")))
      .flatMap(gs => F.fromOption(gs.getAs[SignalSettings]("signal"), AppError.NotSetup("Signal")))

object SignalSettingsRepository:
  def make[F[_]: Async](db: MongoDatabase[F]): F[SignalSettingsRepository[F]] =
    db.getCollectionWithCodec[Document](Repository.Collection.Settings).map(LiveSignalSettingsRepository[F](_))
