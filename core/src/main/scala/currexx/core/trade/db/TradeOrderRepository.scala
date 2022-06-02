package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.trade.TradeOrderPlacement
import currexx.core.common.db.Repository
import currexx.domain.user.UserId
import currexx.domain.market.CurrencyPair
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.Filter
import mongo4cats.database.{CreateCollectionOptions, MongoDatabase}

trait TradeOrderRepository[F[_]] extends Repository[F]:
  def save(top: TradeOrderPlacement): F[Unit]
  def getAll(uid: UserId): F[List[TradeOrderPlacement]]
  def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]]
  def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]]

final private class LiveTradeOrderRepository[F[_]: Async](
    private val collection: MongoCollection[F, TradeOrderEntity]
) extends TradeOrderRepository[F]:
  def save(top: TradeOrderPlacement): F[Unit] =
    collection.insertOne(TradeOrderEntity.from(top)).void
  def getAll(uid: UserId): F[List[TradeOrderPlacement]] =
    collection.find(userIdEq(uid)).sortByDesc("time").all.map(_.map(_.toDomain).toList)
  def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]] =
    collection.distinct[CurrencyPair](Field.CurrencyPair).filter(userIdEq(uid)).all.map(_.toList)

  def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] =
    collection.find(userIdAndCurrencyPairEq(uid, cp)).sortByDesc("time").first.map(_.map(_.toDomain))

object TradeOrderRepository extends MongoJsonCodecs:
  private val collectionName    = "trade-orders"
  private val collectionOptions = CreateCollectionOptions().capped(true).sizeInBytes(268435456L)

  def make[F[_]: Async](db: MongoDatabase[F]): F[TradeOrderRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- if (collNames.toSet.contains(collectionName)) ().pure[F] else db.createCollection(collectionName, collectionOptions)
      coll      <- db.getCollectionWithCodec[TradeOrderEntity](collectionName)
    yield LiveTradeOrderRepository[F](coll.withAddedCodec[CurrencyPair])
