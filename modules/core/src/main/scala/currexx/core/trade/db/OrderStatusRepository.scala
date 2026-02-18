package currexx.core.trade.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.db.Repository.Field
import currexx.core.common.http.SearchParams
import currexx.core.trade.{CurrencyStatistics, EnterOrderStats, ExitOrderStats, OrderStatistics, TimeRange, TradeOrderPlacement}
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

        // Overall stats
        val totalOrders      = orders.size
        val successfulOrders = orders.count(_.status == OrderPlacementStatus.Success)
        val pendingOrders    = orders.count(_.status == OrderPlacementStatus.Pending)
        val cancelledOrders  = orders.count(_.status.isInstanceOf[OrderPlacementStatus.Cancelled])

        // Enter/Exit stats
        val enterOrders    = orders.filter(_.orderKind == "enter")
        val exitOrdersList = orders.filter(_.orderKind == "exit")
        val buyOrders      = enterOrders.count(_.position.contains(TradeOrder.Position.Buy))
        val sellOrders     = enterOrders.count(_.position.contains(TradeOrder.Position.Sell))
        val volumes        = enterOrders.flatMap(_.volume)
        val totalVolume    = volumes.sum
        val avgVolume      = if (volumes.nonEmpty) Some(totalVolume / volumes.size) else None

        val enterStats = EnterOrderStats(
          total = enterOrders.size,
          buyCount = buyOrders,
          sellCount = sellOrders,
          totalVolume = totalVolume,
          averageVolume = avgVolume
        )

        val exitStats = ExitOrderStats(
          total = exitOrdersList.size
        )

        // Currency pair breakdown
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

        // Time range
        val timeRange = if (orders.nonEmpty) {
          val times = orders.map(_.time)
          Some(TimeRange(times.min, times.max))
        } else None

        OrderStatistics(
          totalOrders = totalOrders,
          successfulOrders = successfulOrders,
          pendingOrders = pendingOrders,
          cancelledOrders = cancelledOrders,
          enterOrders = enterStats,
          exitOrders = exitStats,
          currencyBreakdown = currencyBreakdown,
          timeRange = timeRange
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
