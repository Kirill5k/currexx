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
import currexx.domain.errors.AppError
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

  private val unsuccessfulCloses: List[(String, Either[Throwable, OrderPlacementStatus])] = List(
    "cancelled" -> Right(OrderPlacementStatus.Cancelled("MARKET_HALTED")),
    "pending"   -> Right(OrderPlacementStatus.Pending),
    "failed"    -> Left(new RuntimeException("Close response could not be confirmed"))
  )

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

      "save cancelled entry status and fail when broker rejects the requested order" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val cancelReason                                                           = "Insufficient funds"
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Cancelled(cancelReason))
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.attempt.asserting { res =>
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          verifyNoInteractions(dataClient)
          verify(settRepo).get(Users.uid)
          verify(brokerClient).submit(Trades.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Cancelled(cancelReason))
          verify(orderRepo, never).save(any[TradeOrderPlacement])
          verify(orderRepo, never).findLatestBy(any[UserId], any[CurrencyPair])
          disp.submittedActions mustBe empty
          res mustBe Left(AppError.OrderPlacementCancelled(Markets.gbpeur, cancelReason))
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

      List(OrderPlacementStatus.Cancelled("MARKET_HALTED"), OrderPlacementStatus.Pending).foreach { status =>
        s"record $status exits without persisting or publishing an exit" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val order                                                                  = TradeOrder.Exit(Markets.gbpeur, 1.3)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(status)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, false)
          yield ()

          result.attempt.asserting { res =>
            status match
              case OrderPlacementStatus.Cancelled(reason) => res mustBe Left(AppError.OrderPlacementCancelled(Markets.gbpeur, reason))
              case _                                      => res mustBe Right(())
            verify(brokerClient).submit(Settings.trade.broker, order)
            verify(orderStatusRepo).save(placedOrder, status)
            verifyNoInteractions(orderRepo, dataClient)
            disp.submittedActions mustBe empty
          }
        }
      }

      "continue persisting and publishing pending entries" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val order       = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Pending)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.placeOrder(Users.uid, order, false)
        yield ()

        result.asserting { _ =>
          verify(brokerClient).submit(Settings.trade.broker, order)
          verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Pending)
          verify(orderRepo).save(placedOrder)
          verifyNoInteractions(dataClient)
          disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
        }
      }

      unsuccessfulCloses.foreach { case (description, closeResult) =>
        s"not submit an entry after a $description prerequisite close" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val order           = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpeur, 1.3, 0.1)
          val exitOrder       = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedExitOrder = Trades.order.copy(time = now, order = exitOrder)
          when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnSome(Trades.order)
          when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturn(IO.fromEither(closeResult))
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, true)
          yield ()

          result.attempt.asserting { res =>
            closeResult match
              case Right(OrderPlacementStatus.Cancelled(reason)) =>
                res mustBe Left(AppError.OrderPlacementBlocked(Markets.gbpeur, s"prerequisite close was cancelled by broker: $reason"))
              case Right(OrderPlacementStatus.Pending) =>
                res mustBe Left(AppError.OrderPlacementBlocked(Markets.gbpeur, "prerequisite close is still pending"))
              case Left(error)  => res mustBe Left(error)
              case Right(other) => fail(s"Unexpected prerequisite-close status: $other")
            verify(brokerClient).submit(Trades.broker, exitOrder)
            verifyNoMoreInteractions(brokerClient)
            closeResult match
              case Right(status) => verify(orderStatusRepo).save(placedExitOrder, status)
              case Left(_)       => verifyNoInteractions(orderStatusRepo)
            verify(orderRepo, never).save(any[TradeOrderPlacement])
            verifyNoInteractions(settRepo)
            disp.submittedActions mustBe empty
          }
        }
      }

      List(OrderPlacementStatus.Success, OrderPlacementStatus.NoPosition).foreach { closeStatus =>
        s"submit an entry after a $closeStatus prerequisite close" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val order           = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpeur, 1.3, 0.1)
          val exitOrder       = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedExitOrder = Trades.order.copy(time = now, order = exitOrder)
          val placedOrder     = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnSome(Trades.order)
          when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(Trades.broker, exitOrder)).thenReturnIO(closeStatus)
          when(brokerClient.submit(Settings.trade.broker, order)).thenReturnIO(OrderPlacementStatus.Success)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, true)
          yield ()

          result.asserting { _ =>
            verify(brokerClient).submit(Trades.broker, exitOrder)
            verify(brokerClient).submit(Settings.trade.broker, order)
            verify(orderStatusRepo).save(placedExitOrder, closeStatus)
            verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
            if closeStatus == OrderPlacementStatus.Success then verify(orderRepo).save(placedExitOrder)
            else verify(orderRepo, never).save(placedExitOrder)
            verify(orderRepo).save(placedOrder)
            disp.submittedActions mustBe List(
              Action.ProcessTradeOrderPlacement(placedExitOrder),
              Action.ProcessTradeOrderPlacement(placedOrder)
            )
          }
        }
      }

      List(
        "no orders"     -> None,
        "a latest exit" -> Some(Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, 1.5)))
      ).foreach { case (description, latestOrder) =>
        s"submit an entry without closing when there is $description" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnIO(latestOrder)
          when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

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
            verifyNoMoreInteractions(brokerClient)
            verify(orderRepo).findLatestBy(Users.uid, Markets.gbpeur)
            verify(orderRepo).save(placedOrder)
            disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOrder))
            res mustBe ()
          }
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
      val marketProfile = MarketProfile(trend = Some(TrendState(Direction.Downward, Markets.ts)))
      val state         = Markets.state.copy(previousProfile = Some(marketProfile))

      "not do anything when no rules are triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state)
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
        val openLongRule = Rule(TradeAction.OpenLong, Rule.Condition.TrendIs(Direction.Upward))
        val settings     = Settings.trade.copy(strategy = TradeStrategy(List(openLongRule), Nil))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)
        when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementStatus.Success)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val tradeState = state.copy(currentPosition = None)
        val result     = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(tradeState)
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
          _   <- svc.processMarketStateUpdate(state)
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

      List(
        (TradeOrder.Position.Buy, TradeAction.OpenShort, TradeOrder.Position.Sell),
        (TradeOrder.Position.Sell, TradeAction.OpenLong, TradeOrder.Position.Buy)
      ).foreach { case (initialPosition, openAction, targetPosition) =>
        unsuccessfulCloses.foreach { case (description, closeResult) =>
          s"not flip from $initialPosition to $targetPosition after a $description close" in {
            val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
            val openRule        = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
            val settings        = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
            val tradeState      = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
            val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
            val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, settings.broker, now)
            when(settRepo.get(any[UserId])).thenReturnIO(settings)
            when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
            when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturn(IO.fromEither(closeResult))
            when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

            val result = for
              svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
              _   <- svc.processMarketStateUpdate(tradeState)
            yield ()

            result.attempt.asserting { res =>
              res mustBe closeResult.map(_ => ())
              verify(brokerClient).submit(settings.broker, exitOrder)
              verifyNoMoreInteractions(brokerClient)
              closeResult match
                case Right(status) => verify(orderStatusRepo).save(placedExitOrder, status)
                case Left(_)       => verifyNoInteractions(orderStatusRepo)
              verifyNoInteractions(orderRepo)
              disp.submittedActions mustBe empty
            }
          }
        }

        List(OrderPlacementStatus.Success, OrderPlacementStatus.NoPosition).foreach { closeStatus =>
          s"flip from $initialPosition to $targetPosition after a $closeStatus close" in {
            val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
            val openRule        = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
            val settings        = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
            val tradeState      = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
            val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
            val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, settings.broker, now)
            val openOrder       = settings.trading.toOrder(targetPosition, state.currencyPair, Markets.priceRange.close)
            val placedOpenOrder = TradeOrderPlacement(Users.uid, openOrder, settings.broker, now)
            when(settRepo.get(any[UserId])).thenReturnIO(settings)
            when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
            when(brokerClient.submit(settings.broker, exitOrder)).thenReturnIO(closeStatus)
            when(brokerClient.submit(settings.broker, openOrder)).thenReturnIO(OrderPlacementStatus.Success)
            when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
            when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

            val result = for
              svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
              _   <- svc.processMarketStateUpdate(tradeState)
            yield ()

            result.asserting { _ =>
              verify(brokerClient).submit(settings.broker, exitOrder)
              verify(brokerClient).submit(settings.broker, openOrder)
              verify(orderStatusRepo).save(placedExitOrder, closeStatus)
              verify(orderStatusRepo).save(placedOpenOrder, OrderPlacementStatus.Success)
              if closeStatus == OrderPlacementStatus.Success then verify(orderRepo).save(placedExitOrder)
              else verify(orderRepo, never).save(placedExitOrder)
              verify(orderRepo).save(placedOpenOrder)
              disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOpenOrder))
            }
          }
        }
      }

      "do nothing when in long position and open-long rule is triggered" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val openLongRule = Rule(TradeAction.OpenLong, Rule.Condition.TrendIs(Direction.Upward))
        val settings     = Settings.trade.copy(strategy = TradeStrategy(List(openLongRule), Nil))
        when(settRepo.get(any[UserId])).thenReturnIO(settings)

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state)
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

  def mocks: (
      TradeSettingsRepository[IO],
      TradeOrderRepository[IO],
      OrderStatusRepository[IO],
      BrokerClient[IO],
      MarketDataClient[IO],
      MockActionDispatcher[IO]
  ) =
    (
      mock[TradeSettingsRepository[IO]],
      mock[TradeOrderRepository[IO]],
      mock[OrderStatusRepository[IO]],
      mock[BrokerClient[IO]],
      mock[MarketDataClient[IO]],
      MockActionDispatcher[IO]
    )
}
