package currexx.core.trade

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.{BrokerClient, BrokerParameters}
import currexx.clients.data.MarketDataClient
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Settings, Trades, Users}
import currexx.core.market.{MarketProfile, TrendState}
import currexx.core.trade.db.{OrderStatusRepository, TradeOrderRepository, TradeSettingsRepository}
import kirill5k.common.cats.test.IOWordSpec
import currexx.domain.market.{CurrencyPair, OrderPlacementStatus, TradeOrder}
import currexx.domain.monitor.Limits
import currexx.domain.signal.Direction
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant

class TradeServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "A TradeService" when {
    val now         = Instant.now()
    given Clock[IO] = Clock.mock[IO](now)

    "getAllOrders" should {
      "return all orders from the repository" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.getAll(any[UserId], any[SearchParams])).thenReturnIO(List(Trades.order))

        val result = for
          svc    <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo, never).findLatestBy(any[UserId], any[CurrencyPair])
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "save cancelled order status when broker rejects order" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val cancelReason = "Insufficient funds"
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Cancelled(cancelReason))
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Cancelled(cancelReason))
          verify(orderRepo, never).save(any[TradeOrderPlacement])
          verify(orderRepo, never).findLatestBy(any[UserId], any[CurrencyPair])
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "dispatch market state update when broker returns no-position status" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.NoPosition)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val order  = TradeOrder.Exit(Markets.gbpeur, 1.3)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.NoPosition)
          verify(orderRepo, never).save(any[TradeOrderPlacement])
          verify(orderRepo, never).findLatestBy(any[UserId], any[CurrencyPair])
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "close existing orders before submitting the actual placement" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, BigDecimal(1.3), BigDecimal(0.1))
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair]))
          .thenReturnSome(Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, 1.5)))

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnSome(Trades.order)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid, Markets.gbpeur)
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(settRepo)
          verify(dataClient).latestPrice(Markets.gbpeur)
          verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "obtain traded currencies and close all open orders" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(orderRepo.getAllTradedCurrencies(any[UserId])).thenReturnIO(List(Markets.gbpeur, Markets.gbpusd))
        when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnNone

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val cps    = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(None, Some(10), None, None))
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "submit close order if profit is below min" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
          .thenReturnIO(List(Trades.openedOrder.copy(profit = -100)))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val cps    = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None))
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).find(Trades.broker, cps)
          verify(brokerClient).submit(Trades.broker, exitOrder)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "not do anything if profit is within range" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
          .thenReturnIO(List(Trades.openedOrder.copy(profit = 0)))

        val cps    = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None))
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
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(Nil)

        val cps    = NonEmptyList.of(Markets.gbpeur)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOrderIfProfitIsOutsideRange(Users.uid, cps, Limits(Some(-10), Some(10), None, None))
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
      val state         = Markets.state
      val marketProfile = MarketProfile(trend = Some(TrendState(Direction.Downward, Markets.ts)))

      "not do anything when no rules are triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state, marketProfile)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(state.userId)
          verifyNoInteractions(orderRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "open a new long position when not in trade and open-long rule is triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val openLongRule                                          = Rule(TradeAction.OpenLong, Rule.Condition.TrendIs(Direction.Upward))
        val settings                                              = Settings.trade.copy(strategy = TradeStrategy(List(openLongRule), Nil))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val tradeState = state.copy(currentPosition = None)
        val result     = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(tradeState, marketProfile)
        yield ()

        result.asserting { res =>
          val order       = settings.trading.toOrder(TradeOrder.Position.Buy, state.currencyPair, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now)

          verify(settRepo).get(state.userId)
          verify(dataClient).latestPrice(state.currencyPair)
          verify(brokerClient).submit(settings.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "close a long position when in trade and close-long rule is triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val closeRule = Rule(TradeAction.ClosePosition, Rule.Condition.PositionIs(TradeOrder.Position.Buy))
        val settings  = Settings.trade.copy(strategy = TradeStrategy(Nil, List(closeRule)))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state, marketProfile)
        yield ()

        result.asserting { res =>
          val order       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now)

          verify(settRepo).get(state.userId)
          verify(dataClient).latestPrice(state.currencyPair)
          verify(brokerClient).submit(settings.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
          res mustBe ()
        }
      }

      "flip to short when in long position and open-short rule is triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val openShortRule                                         = Rule(TradeAction.OpenShort, Rule.Condition.TrendIs(Direction.Upward))
        val settings = Settings.trade.copy(strategy = TradeStrategy(openRules = List(openShortRule), closeRules = Nil))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state, marketProfile)
        yield ()

        result.asserting { res =>
          val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
          val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now)
          val openOrder       = settings.trading.toOrder(TradeOrder.Position.Sell, state.currencyPair, Markets.priceRange.close)
          val placedOpenOrder = TradeOrderPlacement(Users.uid, openOrder, Trades.broker, now)

          verify(settRepo).get(state.userId)
          verify(dataClient).latestPrice(state.currencyPair)
          verify(brokerClient).submit(settings.broker, exitOrder)
          verify(orderStatusRepo).save(placedExitOrder, OrderPlacementStatus.Success)
          verify(brokerClient).submit(settings.broker, openOrder)
          verify(orderStatusRepo).save(placedOpenOrder, OrderPlacementStatus.Success)
          verify(orderRepo).save(placedExitOrder)
          verify(orderRepo).save(placedOpenOrder)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOpenOrder))
          res mustBe ()
        }
      }

      "do nothing when in long position and open-long rule is triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val openLongRule                                          = Rule(TradeAction.OpenLong, Rule.Condition.TrendIs(Direction.Upward))
        val settings                                              = Settings.trade.copy(strategy = TradeStrategy(List(openLongRule), Nil))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state, marketProfile)
        yield ()

        result.asserting { res =>
          verify(settRepo).get(state.userId)
          verifyNoInteractions(orderRepo, brokerClient, dataClient)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }
  }

  def mocks: (TradeSettingsRepository[IO], TradeOrderRepository[IO], OrderStatusRepository[IO], BrokerClient[IO], MarketDataClient[IO], MockActionDispatcher[IO]) =
    (
      mock[TradeSettingsRepository[IO]],
      mock[TradeOrderRepository[IO]],
      mock[OrderStatusRepository[IO]],
      mock[BrokerClient[IO]],
      mock[MarketDataClient[IO]],
      MockActionDispatcher[IO]
    )
}
