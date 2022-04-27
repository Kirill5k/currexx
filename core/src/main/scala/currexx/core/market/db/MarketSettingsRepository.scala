package currexx.core.market.db

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicative.*
import currexx.clients.broker.BrokerParameters
import currexx.domain.user.UserId
import currexx.core.common.db.Repository
import currexx.core.market.{MarketSettings, TradingParameters}
import currexx.domain.errors.AppError
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.operations.Update
import mongo4cats.database.MongoDatabase
import mongo4cats.collection.MongoCollection

trait MarketSettingsRepository[F[_]] extends Repository[F]:
  def update(settings: MarketSettings): F[Unit]
  def get(uid: UserId): F[MarketSettings]

final private class LiveMarketSettingsRepository[F[_]: Async](
    private val collection: MongoCollection[F, MarketSettingsEntity]
) extends MarketSettingsRepository[F] {

  override def update(settings: MarketSettings): F[Unit] =
    collection
      .updateOne(
        userIdEq(settings.userId),
        Update.set("broker", settings.broker).set("trading", settings.trading)
      )
      .map(_.getMatchedCount)
      .flatMap {
        case 0 => collection.insertOne(MarketSettingsEntity.from(settings)).void
        case _ => ().pure[F]
      }

  override def get(uid: UserId): F[MarketSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(ms => Async[F].fromOption(ms.map(_.toDomain), AppError.NotSetup("Market")))
}

object MarketSettingsRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MarketSettingsRepository[F]] =
    db.getCollectionWithCodec[MarketSettingsEntity]("market-settings")
      .map(coll => LiveMarketSettingsRepository[F](coll.withAddedCodec[TradingParameters].withAddedCodec[BrokerParameters]))
