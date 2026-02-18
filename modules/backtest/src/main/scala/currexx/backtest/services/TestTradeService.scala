package currexx.backtest.services

import cats.Functor
import cats.effect.{Ref, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.SearchParams
import currexx.core.settings.TradeSettings
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.core.trade.{TradeOrderPlacement, TradeService}
import currexx.domain.market.CurrencyPair
import kirill5k.common.cats.Clock
import currexx.domain.user.UserId
import org.typelevel.log4cats.Logger

import scala.collection.mutable.ListBuffer

final private class TestTradeSettingsRepository[F[_]](
    private val settings: Ref[F, TradeSettings]
) extends TradeSettingsRepository[F]:
  override def get(uid: UserId): F[TradeSettings] = settings.get

final private class TestTradeOrderRepository[F[_]: Functor](
    private val orders: Ref[F, ListBuffer[TradeOrderPlacement]]
) extends TradeOrderRepository[F]:
  override def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]]          = orders.get.map(_.map(_.order.currencyPair).distinct.toList)
  override def save(top: TradeOrderPlacement): F[Unit]                             = orders.update(_ :+ top)
  override def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] = orders.get.map(_.toList)
  override def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] = orders.get.map(_.lastOption)

object TestTradeService:
  def make[F[_]: {Temporal, Clock, Logger}](
      initialSettings: TradeSettings,
      clients: TestClients[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    for
      settingsRepo <- Ref.of(initialSettings).map(s => TestTradeSettingsRepository[F](s))
      ordersRepo   <- Ref.of(ListBuffer.empty[TradeOrderPlacement]).map(o => TestTradeOrderRepository[F](o))
      svc          <- TradeService.make(settingsRepo, ordersRepo, clients.broker, clients.data, dispatcher)
    yield svc
