package currexx.core.market.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.domain.market.{CurrencyPair, PriceRange}
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import currexx.core.common.db.Repository
import currexx.core.market.MarketState
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

trait MarketStateRepository[F[_]] extends Repository[F]:
  def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[Unit]
  def getAll(uid: UserId): F[List[MarketState]]
  def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]]

final private class LiveMarketStateRepository[F[_]](
    private val collection: MongoCollection[F, MarketStateEntity]
)(using
  F: Async[F]
) extends MarketStateRepository[F] {

  override def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[Unit] =
    collection
      .updateOne(
        userIdAndCurrencyPairEq(uid, pair),
        Update.set("latestPrice", price).currentTimestamp(Field.LastUpdatedAt)
      )
      .map(_.getMatchedCount)
      .flatMap {
        case 0 => collection.insertOne(MarketStateEntity.make(uid, pair, latestPrice = Some(price))).void
        case _ => ().pure[F]
      }

  override def getAll(uid: UserId): F[List[MarketState]] =
    collection.find(userIdEq(uid)).all.map(_.map(_.toDomain).toList)

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    collection.find(userIdAndCurrencyPairEq(uid, pair)).first.map(_.map(_.toDomain))
}

object MarketStateRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MarketStateRepository[F]] =
    db.getCollectionWithCodec[MarketStateEntity]("market-state")
      .map(coll => LiveMarketStateRepository[F](coll.withAddedCodec[CurrencyPair].withAddedCodec[PriceRange]))
