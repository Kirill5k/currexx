package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.trade.TradeOrderPlacement
import currexx.core.common.db.Repository
import currexx.core.common.db.Repository.Field
import currexx.core.common.http.SearchParams
import currexx.domain.user.UserId
import currexx.domain.market.CurrencyPair
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Index}
import mongo4cats.models.database.CreateCollectionOptions
import mongo4cats.database.MongoDatabase

trait TradeOrderRepository[F[_]] extends Repository[F]:
  def save(top: TradeOrderPlacement): F[Unit]
  def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]
  def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]]
  def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]]

final private class LiveTradeOrderRepository[F[_]: Async](
    private val collection: MongoCollection[F, TradeOrderEntity]
) extends TradeOrderRepository[F]:

  def save(top: TradeOrderPlacement): F[Unit] =
    collection.insertOne(TradeOrderEntity.from(top)).void

  def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]] =
    collection
      .distinct[CurrencyPair](Field.OrderCurrencyPair)
      .filter(userIdEq(uid))
      .all
      .map(_.toList)

  def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] =
    collection
      .find(searchBy(uid, sp, Field.OrderCurrencyPair))
      .sortByDesc(Field.Time)
      .limit(sp.limit.getOrElse(Int.MaxValue))
      .all
      .mapIterable(_.toDomain)

  def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] =
    collection
      .find(userIdEq(uid) && Filter.eq(Field.OrderCurrencyPair, cp))
      .sortByDesc(Field.Time)
      .first
      .mapOption(_.toDomain)

object TradeOrderRepository extends MongoJsonCodecs:
  private val collectionName    = Repository.Collection.TradeOrders
  private val collectionOptions = CreateCollectionOptions(capped = true, sizeInBytes = 268435456L)

  val indexByUid       = Index.ascending(Field.UId)
  val indexByCp        = indexByUid.combinedWith(Index.ascending(Field.OrderCurrencyPair))
  val indexByCpAndTime = indexByCp.combinedWith(Index.ascending(Field.Time))

  def make[F[_]](db: MongoDatabase[F])(using F: Async[F]): F[TradeOrderRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- F.unlessA(collNames.toSet.contains(collectionName))(db.createCollection(collectionName, collectionOptions))
      coll      <- db.getCollectionWithCodec[TradeOrderEntity](collectionName)
      _         <- coll.createIndex(indexByUid)
      _         <- coll.createIndex(indexByCp)
      _         <- coll.createIndex(indexByCpAndTime)
    yield LiveTradeOrderRepository[F](coll.withAddedCodec[CurrencyPair])
