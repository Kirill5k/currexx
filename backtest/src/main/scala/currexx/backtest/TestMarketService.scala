package currexx.backtest

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.action.ActionDispatcher
import currexx.domain.user.UserId
import currexx.core.market.{IndicatorState, MarketService, MarketState}
import currexx.core.market.db.MarketStateRepository
import currexx.domain.market.{CurrencyPair, Indicator, MarketTimeSeriesData, PriceRange, TradeOrder}

final private class TestMarketStateRepository[F[_]: Monad](
    private val state: Ref[F, MarketState]
) extends MarketStateRepository[F]:
  override def deleteAll(uid: UserId): F[Unit] = Monad[F].unit
  override def update(uid: UserId, pair: CurrencyPair, signals: Map[Indicator, List[IndicatorState]]): F[MarketState] =
    state.updateAndGet(_.copy(signals = signals))
  override def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[MarketState] =
    state.updateAndGet(_.copy(latestPrice = Some(price)))
  override def update(uid: UserId, pair: CurrencyPair, position: Option[TradeOrder.Position]): F[MarketState] =
    state.updateAndGet(_.copy(currentPosition = position))
  override def getAll(uid: UserId): F[List[MarketState]] =
    state.get.map(List(_))
  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    state.get.map(Some(_))

object TestMarketService:
  def make[F[_]: Async](initialState: MarketState, dispatcher: ActionDispatcher[F]): F[MarketService[F]] =
    Ref.of(initialState).flatMap(s => MarketService.make(TestMarketStateRepository(s), dispatcher))
