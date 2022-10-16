package currexx.backtest.services

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.SearchParams
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.core.trade.{TradeOrderPlacement, TradeService, TradeSettings}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange, TradeOrder}
import currexx.domain.time.Clock
import currexx.domain.user.UserId

final private class TestTradeSettingsRepository[F[_]](
    private val settings: Ref[F, TradeSettings]
) extends TradeSettingsRepository[F]:
  override def update(ts: TradeSettings): F[Unit] = settings.update(_ => ts)
  override def get(uid: UserId): F[TradeSettings] = settings.get

final private class TestTradeOrderRepository[F[_]: Async](
    private val orders: Ref[F, List[TradeOrderPlacement]]
) extends TradeOrderRepository[F]:
  override def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]]          = orders.get.map(_.headOption.map(_.order.currencyPair).toList)
  override def save(top: TradeOrderPlacement): F[Unit]                             = orders.update(top :: _)
  override def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]] = orders.get
  override def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] = orders.get.map(_.headOption)

object TestTradeService:
  def make[F[_]: Async: Clock](
      initialSettings: TradeSettings,
      clients: TestClients[F],
      dispatcher: ActionDispatcher[F]
  ): F[TradeService[F]] =
    for
      settingsRepo <- Ref.of(initialSettings).map(s => TestTradeSettingsRepository[F](s))
      ordersRepo   <- Ref.of(List.empty[TradeOrderPlacement]).map(o => TestTradeOrderRepository[F](o))
      svc          <- TradeService.make(settingsRepo, ordersRepo, clients.broker, clients.data, dispatcher)
    yield svc
