package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.db.Repository.Field
import currexx.core.common.http.SearchParams
import currexx.core.trade.{CurrencyStatistics, EnterOrderStats, OrderStatistics, TradeOrderPlacement}
import currexx.domain.market.{CurrencyPair, OrderPlacementStatus, TradeOrder}
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.Index
import mongo4cats.database.MongoDatabase

trait OrderStatusRepository[F[_]] extends Repository[F]:
  def save(top: TradeOrderPlacement, status: OrderPlacementStatus): F[Unit]
  def getStatistics(uid: UserId, sp: SearchParams): F[OrderStatistics]

final private class LiveOrderStatusRepository[F[_]: Async](
    private val collection: MongoCollection[F, OrderStatusEntity]
) extends OrderStatusRepository[F]:

  def save(top: TradeOrderPlacement, status: OrderPlacementStatus): F[Unit] =
    collection.insertOne(OrderStatusEntity.from(top, status)).void

  def getStatistics(uid: UserId, sp: SearchParams): F[OrderStatistics] =
    // Fetch all matching orders and compute stats in application code
    // (MongoDB aggregation pipeline would be more efficient but more complex)
    collection
      .find(searchBy(uid, sp, Field.CurrencyPair))
      .all
      .map { entities =>
        val orders = entities.toList

        val enterOrders = orders.filter(_.orderKind == "enter")
        val volumes     = enterOrders.flatMap(_.volume)
        val totalVolume = volumes.sum

        val currencyBreakdown = orders
          .groupBy(_.currencyPair)
          .map { case (cp, cpOrders) =>
            val cpEnterOrders = cpOrders.filter(_.orderKind == "enter")
            CurrencyStatistics(
              currencyPair = cp,
              totalOrders = cpOrders.size,
              successfulOrders = cpOrders.count(_.status == OrderPlacementStatus.Success),
              pendingOrders = cpOrders.count(_.status == OrderPlacementStatus.Pending),
              cancelledOrders = cpOrders.count(_.status.isInstanceOf[OrderPlacementStatus.Cancelled]),
              enterOrders = cpEnterOrders.size,
              exitOrders = cpOrders.count(_.orderKind == "exit"),
              buyOrders = cpEnterOrders.count(_.position.contains(TradeOrder.Position.Buy)),
              sellOrders = cpEnterOrders.count(_.position.contains(TradeOrder.Position.Sell)),
              totalVolume = cpEnterOrders.flatMap(_.volume).sum
            )
          }
          .toList
          .sortBy(-_.totalOrders) // Sort by most active pairs

        OrderStatistics(
          totalOrders = orders.size,
          successfulOrders = orders.count(_.status == OrderPlacementStatus.Success),
          pendingOrders = orders.count(_.status == OrderPlacementStatus.Pending),
          cancelledOrders = orders.count(_.status.isInstanceOf[OrderPlacementStatus.Cancelled]),
          enterOrders = EnterOrderStats(
            total = enterOrders.size,
            buyCount = enterOrders.count(_.position.contains(TradeOrder.Position.Buy)),
            sellCount = enterOrders.count(_.position.contains(TradeOrder.Position.Sell)),
            totalVolume = totalVolume,
            averageVolume = Option.when(volumes.nonEmpty)(totalVolume / volumes.size)
          ),
          exitOrders = orders.count(_.orderKind == "exit"),
          currencyBreakdown = currencyBreakdown
        )
      }

object OrderStatusRepository extends MongoJsonCodecs:
  private val indexByUid    = Index.ascending(Field.UId)
  private val indexByCp     = indexByUid.combinedWith(Index.ascending(Field.CurrencyPair))
  private val indexByTime   = Index.ascending(Field.Time)
  private val indexByStatus = Index.ascending(Field.Status)

  def make[F[_]](db: MongoDatabase[F])(using F: Async[F]): F[OrderStatusRepository[F]] =
    for
      coll <- db.getCollectionWithCodec[OrderStatusEntity](Repository.Collection.OrderStatus)
      _    <- coll.createIndex(indexByUid)
      _    <- coll.createIndex(indexByCp)
      _    <- coll.createIndex(indexByTime)
      _    <- coll.createIndex(indexByStatus)
    yield LiveOrderStatusRepository[F](coll.withAddedCodec[CurrencyPair])
