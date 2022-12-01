package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.settings.SignalParameters
import currexx.domain.market.{CurrencyPair, Indicator}
import currexx.domain.user.UserId
import currexx.core.signal.SignalSettings
import currexx.domain.errors.AppError
import mongo4cats.bson.Document
import mongo4cats.circe.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

import scala.jdk.CollectionConverters.*

trait SignalSettingsRepository[F[_]] extends Repository[F]:
  def get(uid: UserId): F[SignalParameters]

final private class LiveSignalSettingsRepository[F[_]](
    private val collection: MongoCollection[F, Document]
)(using
    F: Async[F]
) extends SignalSettingsRepository[F] {

  override def get(uid: UserId): F[SignalParameters] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(gs => F.fromOption(gs, AppError.NotSetup("Global")))
      .flatMap(gs => F.fromOption(gs.getAs[SignalParameters]("signal"), AppError.NotSetup("Signal")))
}

object SignalSettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[SignalSettingsRepository[F]] =
    db.getCollectionWithCodec[Document]("settings").map(LiveSignalSettingsRepository[F](_))
