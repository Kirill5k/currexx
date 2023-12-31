package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.db.Repository
import currexx.core.settings.TradeSettings
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.bson.Document
import mongo4cats.circe.*
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase

trait TradeSettingsRepository[F[_]] extends Repository[F]:
  def get(uid: UserId): F[TradeSettings]

final private class LiveTradeSettingsRepository[F[_]](
    private val collection: MongoCollection[F, Document]
)(using
    F: Async[F]
) extends TradeSettingsRepository[F]:
  override def get(uid: UserId): F[TradeSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(gs => F.fromOption(gs, AppError.NotSetup("Global")))
      .flatMap(gs => F.fromOption(gs.getAs[TradeSettings]("trade"), AppError.NotSetup("Trade")))

object TradeSettingsRepository:
  def make[F[_]: Async](db: MongoDatabase[F]): F[TradeSettingsRepository[F]] =
    db.getCollectionWithCodec[Document]("settings")
      .map(LiveTradeSettingsRepository[F](_))
