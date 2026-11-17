package currexx.backtest.services

import cats.{Functor, Monad}
import cats.effect.Ref
import cats.syntax.functor.*
import currexx.core.common.http.SearchParams
import currexx.core.settings.TradeSettings
import currexx.core.trade.db.{OrderStatusRepository, TradeOrderRepository, TradeSettingsRepository}
import currexx.core.trade.{EnterOrderStats, OrderStatistics, TradeOrderPlacement}
import currexx.domain.market.{CurrencyPair, OrderPlacementStatus}
import currexx.domain.user.UserId

import scala.collection.mutable.ListBuffer

final private class TestTradeSettingsRepository[F[_]](
    private val settings: Ref[F, TradeSettings]
) extends TradeSettingsRepository[F]:
  override def get(uid: UserId): F[TradeSettings] = settings.get

final private class TestTradeOrderRepository[F[_]: Functor](
    private val orders: Ref[F, ListBuffer[TradeOrderPlacement]]
) extends TradeOrderRepository[F]:
  override def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]] = orders.get.map(_.map(_.order.currencyPair).distinct.toList)
  override def save(top: TradeOrderPlacement): F[Unit]                    = orders.update(_ :+ top)
  override def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]         = orders.get.map(_.toList)
  override def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] = orders.get.map(_.lastOption)

final private class TestOrderStatusRepository[F[_]](using F: Monad[F]) extends OrderStatusRepository[F]:
  override def save(top: TradeOrderPlacement, status: OrderPlacementStatus): F[Unit] = F.unit
  override def getStatistics(uid: UserId, sp: SearchParams): F[OrderStatistics]      = F.pure {
    OrderStatistics(
      totalOrders = 0,
      successfulOrders = 0,
      pendingOrders = 0,
      cancelledOrders = 0,
      noPositionOrders = 0,
      enterOrders = EnterOrderStats(
        total = 0,
        successCount = 0,
        buyCount = 0,
        sellCount = 0,
        totalVolume = 0.0,
        averageVolume = None
      ),
      exitOrders = 0,
      currencyBreakdown = Nil
    )
  }
