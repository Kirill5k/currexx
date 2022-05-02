package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.BrokerParameters
import currexx.core.common.db.Repository
import currexx.core.trade.db.{TradeSettingsEntity, TradeSettingsRepository}
import currexx.core.trade.{TradeSettings, TradeStrategy, TradingParameters}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.Update
import mongo4cats.database.MongoDatabase

trait TradeSettingsRepository[F[_]] extends Repository[F]:
  def update(settings: TradeSettings): F[Unit]
  def get(uid: UserId): F[TradeSettings]

final private class LiveTradeSettingsRepository[F[_]: Async](
    private val collection: MongoCollection[F, TradeSettingsEntity]
) extends TradeSettingsRepository[F] {

  override def update(settings: TradeSettings): F[Unit] =
    collection
      .updateOne(
        userIdEq(settings.userId),
        Update.set("broker", settings.broker).set("trading", settings.trading).set("strategy", settings.strategy)
      )
      .map(_.getMatchedCount)
      .flatMap {
        case 0 => collection.insertOne(TradeSettingsEntity.from(settings)).void
        case _ => ().pure[F]
      }

  override def get(uid: UserId): F[TradeSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(ms => Async[F].fromOption(ms.map(_.toDomain), AppError.NotSetup("Trade")))
}

object TradeSettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[TradeSettingsRepository[F]] =
    db.getCollectionWithCodec[TradeSettingsEntity]("trade-settings")
      .map(_.withAddedCodec[TradingParameters].withAddedCodec[BrokerParameters].withAddedCodec[TradeStrategy])
      .map(coll => LiveTradeSettingsRepository[F](coll))
