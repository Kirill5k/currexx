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
import currexx.domain.user.UserId

final private class TestTradeSettingsRepository[F[_]](
    private val settings: Ref[F, TradeSettings]
) extends TradeSettingsRepository[F]:
  override def update(ts: TradeSettings): F[Unit] = settings.update(_ => ts)
  override def get(uid: UserId): F[TradeSettings] = settings.get

final private class TestTradeOrderRepository[F[_]: Async](
    private val orders: Ref[F, List[TradeOrderPlacement]]
) extends TradeOrderRepository[F]:
  override def getAllTradedCurrencies(uid: UserId): F[List[CurrencyPair]]                  = Async[F].pure(Nil)
  override def save(top: TradeOrderPlacement): F[Unit]                                     = orders.update(top :: _)
  override def getAll(uid: UserId, sp: SearchParams): F[List[TradeOrderPlacement]]         = orders.get
  override def findLatestBy(uid: UserId, cp: CurrencyPair): F[Option[TradeOrderPlacement]] = Async[F].pure(None)

final private class TestBrokerClient[F[_]](using F: Monad[F]) extends BrokerClient[F]:
  override def submit(pair: CurrencyPair, parameters: BrokerParameters, order: TradeOrder): F[Unit] = F.unit

final private class TestMarketDataClient[F[_]](using F: Monad[F]) extends MarketDataClient[F]:
  override def timeSeriesData(currencyPair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] = ???
  override def latestPrice(currencyPair: CurrencyPair): F[PriceRange]                                  = ???

object TestTradeService:
  def make[F[_]: Async](initialSettings: TradeSettings, dispatcher: ActionDispatcher[F]): F[TradeService[F]] =
    for
      settingsRepo <- Ref.of(initialSettings).map(s => TestTradeSettingsRepository[F](s))
      ordersRepo   <- Ref.of(List.empty[TradeOrderPlacement]).map(o => TestTradeOrderRepository[F](o))
      svc          <- TradeService.make(settingsRepo, ordersRepo, TestBrokerClient[F], TestMarketDataClient[F], dispatcher)
    yield svc
