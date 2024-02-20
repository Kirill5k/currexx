package currexx.core.trade

import cats.effect.IO
import cats.data.NonEmptyList
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.core.{MockActionDispatcher, MockClock}
import currexx.core.common.action.Action
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Settings, Trades, Users}
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import currexx.domain.user.UserId
import currexx.domain.market.{CurrencyPair, IndicatorKind, TradeOrder}
import currexx.domain.monitor.Limits
import currexx.domain.time.Clock
import currexx.domain.IOWordSpec

import java.time.Instant

class TradeServiceSpec extends IOWordSpec {

  "A TradeService" when {
    val now         = Instant.now()
    given Clock[IO] = MockClock[IO](now)

    "getAllOrders" should {
      "return all orders from the repository" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.getAll(any[UserId], any[SearchParams])).thenReturnIO(List(Trades.order))

        val result = for
          svc    <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          orders <- svc.getAllOrders(Users.uid, SearchParams(None, Some(Trades.ts)))
        yield orders

        result.asserting { res =>
          verify(orderRepo).getAll(Users.uid, SearchParams(None, Some(Trades.ts)))
          verifyNoInteractions(settRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe List(Trades.order)
        }
      }
    }

    "placeOrder" should {
      "submit order placements" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val order = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderRepo, never).findLatestBy(any[UserId], any[CurrencyPair])
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "close existing orders before submitting the actual placement" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val order = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, BigDecimal(1.3), BigDecimal(0.1))
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, true)
        yield ()

        result.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }
    }

    "closeOpenOrders" should {
      "not do anything when there are no orders" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid, Markets.gbpeur)
        yield ()

        result.asserting { res =>
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verifyNoInteractions(settRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "not do anything when latest order is exit" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair]))
          .thenReturnSome(Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, 1.5)))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid, Markets.gbpeur)
        yield ()

        result.asserting { res =>
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verifyNoInteractions(settRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "close existing order" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnSome(Trades.order)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid, Markets.gbpeur)
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(settRepo)
          verify(dataClient).latestPrice(Markets.gbpeur)
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "obtain traded currencies and close all open orders" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.getAllTradedCurrencies(any[UserId])).thenReturnIO(List(Markets.gbpeur, Markets.gbpusd))
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid)
        yield ()

        result.asserting { res =>
          verify(orderRepo).getAllTradedCurrencies(Users.uid)
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpusd)
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verifyNoInteractions(settRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "closeOrderIfProfitIsOutsideRange" should {
      "submit close order if profit is above max" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val cps = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(None, Some(10), None, None, false))
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "submit close and open orders if profit is above max and is trailing" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val cps = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(None, Some(10), None, None, true))
        yield ()

        result.asserting { res =>
          val exitOrder        = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val enterOrder       = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, Markets.priceRange.close, BigDecimal(0.1))
          val placedExitOrder  = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          val placedEnterOrder = TradeOrderPlacement(Users.uid, enterOrder, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(brokerClient).submit(Trades.broker, enterOrder)
          verify(orderRepo).save(placedExitOrder)
          verify(orderRepo).save(placedEnterOrder)
          disp.submittedActions mustBe List(
            Action.ProcessTradeOrderPlacement(placedExitOrder),
            Action.ProcessTradeOrderPlacement(placedEnterOrder)
          )
          res mustBe ()
        }
      }

      "submit close order if profit is below min" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
          .thenReturnIO(List(Trades.openedOrder.copy(profit = -100)))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val cps = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None, false))
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "not do anything if profit is within range" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
          .thenReturnIO(List(Trades.openedOrder.copy(profit = 0)))

        val cps = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None, false))
        yield ()

        result.asserting { res =>
          verifyNoInteractions(dataClient, orderRepo)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verifyNoMoreInteractions(brokerClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "not do anything if there are no opened positions" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(Nil)

        val cps = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None, false))
        yield ()

        result.asserting { res =>
          verifyNoInteractions(dataClient, orderRepo)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verifyNoMoreInteractions(brokerClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "processMarketStateUpdate" should {
      "not do anything when trading strategy is disabled" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade.copy(strategy = TradeStrategy.Disabled))
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(Markets.state, List(IndicatorKind.TrendChangeDetection))
        yield ()

        result.asserting { res =>
          verify(settRepo).get(Users.uid)
          verify(dataClient).latestPrice(Markets.gbpeur)
          verifyNoInteractions(orderRepo, brokerClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "close existing order if new order has reverse position" in {
        val (settRepo, orderRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade.copy(strategy = TradeStrategy.TrendChange))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, brokerClient, dataClient, disp)
          currentState = Markets.stateWithSignal.copy(currentPosition =
            Some(Markets.positionState.copy(position = TradeOrder.Position.Sell))
          )
          _ <- svc.processMarketStateUpdate(currentState, List(IndicatorKind.TrendChangeDetection))
        yield ()

        result.asserting { res =>
          val order       = Settings.trade.trading.toOrder(TradeOrder.Position.Buy, Markets.gbpeur, 3.0)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, TradeOrder.Exit(Markets.gbpeur, 3.0))
          verify(brokerClient).submit(Trades.broker, order)
          verify(dataClient).latestPrice(Markets.gbpeur)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }
    }
  }

  def mocks: (TradeSettingsRepository[IO], TradeOrderRepository[IO], BrokerClient[IO], MarketDataClient[IO], MockActionDispatcher[IO]) =
    (
      mock[TradeSettingsRepository[IO]],
      mock[TradeOrderRepository[IO]],
      mock[BrokerClient[IO]],
      mock[MarketDataClient[IO]],
      MockActionDispatcher[IO]
    )
}
