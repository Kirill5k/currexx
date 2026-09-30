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
import currexx.domain.market.{CurrencyPair, OrderExecution, OrderPlacementResult, OrderPlacementStatus, OrderRef, TradeOrder}
import currexx.domain.monitor.Limits
import currexx.domain.signal.Direction
import currexx.domain.user.UserId
import kirill5k.common.cats.Clock
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import java.time.Instant

class TradeServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val exitExecution = Trades.execution.copy(orderId = "44", transactionId = "45")
  private val entryFill     = OrderPlacementResult.filled(Trades.execution)
  private val exitFill      = OrderPlacementResult.filled(exitExecution)
  private val pendingOrder  = OrderPlacementResult.Pending(OrderRef("client-order-id", Some("42")))
  // The broker's position once an entry for Trades.openedOrder has filled
  private val filledPosition = BrokerPosition.Open(Trades.openedOrder.position, Trades.openedOrder.openPrice)

  private val unsuccessfulCloses: List[(String, Either[Throwable, OrderPlacementResult])] = List(
    "cancelled" -> Right(OrderPlacementResult.Cancelled("MARKET_HALTED")),
    "pending"   -> Right(pendingOrder),
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
      for
        (fillDescription, brokerResult) <- List(
          "their execution"                  -> entryFill,
          "a fill without execution details" -> OrderPlacementResult.FilledWithoutExecution(Some("42"))
        )
        (positionDescription, positionLookup, brokerPosition) <- List(
          ("the broker's position", IO.pure(List(Trades.openedOrder)), filledPosition),
          ("a flat broker position", IO.pure(Nil), BrokerPosition.Flat),
          ("a broker position without an average price", IO.pure(List(Trades.openedOrder.copy(openPrice = 0))), BrokerPosition.Unknown),
          ("an unreadable broker position", IO.raiseError(new RuntimeException("Positions unavailable")), BrokerPosition.Unknown)
        )
      do
        s"submit order placements and record $fillDescription with $positionDescription" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(brokerResult)
          when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturn(positionLookup)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

          val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, false)
          yield ()

          result.asserting { res =>
            val placedOrder =
              TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now, brokerResult.executions, Some(brokerPosition))
            verifyNoInteractions(dataClient)
            verify(settRepo).get(Users.uid)
            verify(brokerClient).submit(Trades.broker, order)
            verify(brokerClient).find(Trades.broker, NonEmptyList.one(Markets.gbpeur))
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
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementResult.Cancelled(cancelReason))
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
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(OrderPlacementResult.NoPosition)
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

      List(OrderPlacementResult.Cancelled("MARKET_HALTED"), pendingOrder).foreach { brokerResult =>
        s"record ${brokerResult.status} exits without persisting or publishing an exit" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val order                                                                  = TradeOrder.Exit(Markets.gbpeur, 1.3)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(brokerResult)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, false)
          yield ()

          result.attempt.asserting { res =>
            brokerResult match
              case OrderPlacementResult.Cancelled(reason) => res mustBe Left(AppError.OrderPlacementCancelled(Markets.gbpeur, reason))
              case _                                      => res mustBe Right(())
            verify(brokerClient).submit(Settings.trade.broker, order)
            verify(orderStatusRepo).save(placedOrder, brokerResult.status)
            verifyNoInteractions(orderRepo, dataClient)
            disp.submittedActions mustBe empty
          }
        }
      }

      "continue persisting and publishing pending entries without an execution" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        val order       = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, 1.3, 0.1)
        val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now)
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(pendingOrder)
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
              case Right(OrderPlacementResult.Cancelled(reason)) =>
                res mustBe Left(AppError.OrderPlacementBlocked(Markets.gbpeur, s"prerequisite close was cancelled by broker: $reason"))
              case Right(OrderPlacementResult.Pending(_)) =>
                res mustBe Left(AppError.OrderPlacementBlocked(Markets.gbpeur, "prerequisite close is still pending"))
              case Left(error)  => res mustBe Left(error)
              case Right(other) => fail(s"Unexpected prerequisite-close status: $other")
            verify(brokerClient).submit(Trades.broker, exitOrder)
            verifyNoMoreInteractions(brokerClient)
            closeResult match
              case Right(brokerResult) => verify(orderStatusRepo).save(placedExitOrder, brokerResult.status)
              case Left(_)             => verifyNoInteractions(orderStatusRepo)
            verify(orderRepo, never).save(any[TradeOrderPlacement])
            verifyNoInteractions(settRepo)
            disp.submittedActions mustBe empty
          }
        }
      }

      List(exitFill, OrderPlacementResult.NoPosition).foreach { closeResult =>
        s"submit an entry after a ${closeResult.status} prerequisite close" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val order           = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpeur, 1.3, 0.1)
          val exitOrder       = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedExitOrder = Trades.order.copy(time = now, order = exitOrder, executions = closeResult.executions)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Settings.trade.broker, now, List(Trades.execution), Some(filledPosition))
          when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnSome(Trades.order)
          when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.submit(Trades.broker, exitOrder)).thenReturnIO(closeResult)
          when(brokerClient.submit(Settings.trade.broker, order)).thenReturnIO(entryFill)
          when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, true)
          yield ()

          result.asserting { _ =>
            verify(brokerClient).submit(Trades.broker, exitOrder)
            verify(brokerClient).submit(Settings.trade.broker, order)
            verify(orderStatusRepo).save(placedExitOrder, closeResult.status)
            verify(orderStatusRepo).save(placedOrder, OrderPlacementStatus.Success)
            if closeResult == exitFill then verify(orderRepo).save(placedExitOrder)
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
          when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(entryFill)
          when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.findLatestBy(any[UserId], any[CurrencyPair])).thenReturnIO(latestOrder)
          when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

          val order  = TradeOrder.Enter(TradeOrder.Position.Buy, Markets.gbpeur, BigDecimal(1.3), BigDecimal(0.1))
          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            _   <- svc.placeOrder(Users.uid, order, true)
          yield ()

          result.asserting { res =>
            val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now, List(Trades.execution), Some(filledPosition))
            verifyNoInteractions(dataClient)
            verify(settRepo).get(Users.uid)
            verify(brokerClient).submit(Trades.broker, order)
            verify(brokerClient).find(Trades.broker, NonEmptyList.one(Markets.gbpeur))
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
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(exitFill)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.closeOpenOrders(Users.uid, Markets.gbpeur)
        yield ()

        result.asserting { res =>
          val exitOrder   = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now, List(exitExecution))
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

    "findBrokerPosition" should
      List(
        ("the broker's position", IO.pure(List(Trades.openedOrder)), filledPosition),
        ("a flat broker position", IO.pure(Nil), BrokerPosition.Flat),
        ("a broker position without an average price", IO.pure(List(Trades.openedOrder.copy(openPrice = 0))), BrokerPosition.Unknown),
        ("an unreadable broker position", IO.raiseError(new RuntimeException("Positions unavailable")), BrokerPosition.Unknown)
      ).foreach { case (description, positionLookup, expected) =>
        s"return $description" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
          when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturn(positionLookup)

          val result = for
            svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            res <- svc.findBrokerPosition(Users.uid, Markets.gbpeur)
          yield res

          result.asserting { res =>
            verify(settRepo).get(Users.uid)
            verify(brokerClient).find(Trades.broker, NonEmptyList.one(Markets.gbpeur))
            verifyNoMoreInteractions(brokerClient)
            verifyNoInteractions(orderRepo, orderStatusRepo, dataClient)
            disp.submittedActions mustBe empty
            res mustBe expected
          }
        }
      }

    "closeOrderIfProfitIsOutsideRange" should {
      "submit close order if profit is above max" in {
        val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
        when(settRepo.get(any[UserId])).thenReturnIO(Settings.trade)
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(Trades.openedOrder))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(exitFill)
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
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now, List(exitExecution))
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
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(exitFill)
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
          val placedOrder = TradeOrderPlacement(Users.uid, exitOrder, Trades.broker, now, List(exitExecution))
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
        when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
          .thenReturn(IO.pure(Nil), IO.pure(List(Trades.openedOrder)))
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(entryFill)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val tradeState = state.copy(currentPosition = None)
        val result     = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(tradeState)
        yield ()

        result.asserting { res =>
          val order       = settings.trading.toOrder(TradeOrder.Position.Buy, state.currencyPair, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now, List(Trades.execution), Some(filledPosition))

          verify(settRepo).get(state.userId)
          verify(dataClient).latestPrice(state.currencyPair)
          verify(brokerClient, times(2)).find(settings.broker, NonEmptyList.one(state.currencyPair))
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
        when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturnIO(exitFill)
        when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
        when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

        val result = for
          svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
          _   <- svc.processMarketStateUpdate(state)
        yield ()

        result.asserting { res =>
          val order       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
          val placedOrder = TradeOrderPlacement(Users.uid, order, Trades.broker, now, List(exitExecution))

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
            when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(Nil)
            when(brokerClient.submit(any[BrokerParameters], any[TradeOrder])).thenReturn(IO.fromEither(closeResult))
            when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit

            val result = for
              svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
              _   <- svc.processMarketStateUpdate(tradeState)
            yield ()

            result.attempt.asserting { res =>
              res mustBe closeResult.map(_ => ())
              verify(brokerClient).find(settings.broker, NonEmptyList.one(state.currencyPair))
              verify(brokerClient).submit(settings.broker, exitOrder)
              verifyNoMoreInteractions(brokerClient)
              closeResult match
                case Right(brokerResult) => verify(orderStatusRepo).save(placedExitOrder, brokerResult.status)
                case Left(_)             => verifyNoInteractions(orderStatusRepo)
              verifyNoInteractions(orderRepo)
              disp.submittedActions mustBe empty
            }
          }
        }

        List(exitFill, OrderPlacementResult.NoPosition).foreach { closeResult =>
          val closeStatus = closeResult.status
          List(entryFill, pendingOrder).foreach { entryResult =>
            s"publish only the ${entryResult.status} entry when flipping from $initialPosition to $targetPosition after a $closeStatus close" in {
              val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
              val openRule        = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
              val settings        = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
              val tradeState      = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
              val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
              val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, settings.broker, now, closeResult.executions)
              val openOrder       = settings.trading.toOrder(targetPosition, state.currencyPair, Markets.priceRange.close)
              val openedOrder     = Trades.openedOrder.copy(position = targetPosition)
              val brokerPosition  = Option.when(entryResult == entryFill)(BrokerPosition.from(openedOrder))
              val placedOpenOrder = TradeOrderPlacement(Users.uid, openOrder, settings.broker, now, entryResult.executions, brokerPosition)
              when(settRepo.get(any[UserId])).thenReturnIO(settings)
              when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
              when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
                .thenReturn(IO.pure(Nil), IO.pure(List(openedOrder)))
              when(brokerClient.submit(settings.broker, exitOrder)).thenReturnIO(closeResult)
              when(brokerClient.submit(settings.broker, openOrder)).thenReturnIO(entryResult)
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
                verify(orderStatusRepo).save(placedOpenOrder, entryResult.status)
                if closeStatus == OrderPlacementStatus.Success then verify(orderRepo).save(placedExitOrder)
                else verify(orderRepo, never).save(placedExitOrder)
                verify(orderRepo).save(placedOpenOrder)
                disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedOpenOrder))
              }
            }
          }

          List[(String, Either[Throwable, OrderPlacementResult])](
            "cancelled" -> Right(OrderPlacementResult.Cancelled("MARKET_HALTED")),
            "failed"    -> Left(new RuntimeException("Entry response could not be confirmed"))
          ).foreach { case (description, entryResult) =>
            s"publish the $closeStatus exit when the flip from $initialPosition to $targetPosition has a $description entry" in {
              val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
              val openRule        = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
              val settings        = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
              val tradeState      = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
              val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
              val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, settings.broker, now, closeResult.executions)
              val openOrder       = settings.trading.toOrder(targetPosition, state.currencyPair, Markets.priceRange.close)
              val placedOpenOrder = TradeOrderPlacement(Users.uid, openOrder, settings.broker, now)
              when(settRepo.get(any[UserId])).thenReturnIO(settings)
              when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
              when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(Nil)
              when(brokerClient.submit(settings.broker, exitOrder)).thenReturnIO(closeResult)
              when(brokerClient.submit(settings.broker, openOrder)).thenReturn(IO.fromEither(entryResult))
              when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
              when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

              val result = for
                svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
                _   <- svc.processMarketStateUpdate(tradeState)
              yield ()

              result.attempt.asserting { res =>
                res mustBe entryResult.map(_ => ())
                verify(brokerClient).find(settings.broker, NonEmptyList.one(state.currencyPair))
                verify(brokerClient).submit(settings.broker, exitOrder)
                verify(brokerClient).submit(settings.broker, openOrder)
                verifyNoMoreInteractions(brokerClient)
                verify(orderStatusRepo).save(placedExitOrder, closeStatus)
                entryResult match
                  case Right(brokerResult) => verify(orderStatusRepo).save(placedOpenOrder, brokerResult.status)
                  case Left(_)             => verifyNoMoreInteractions(orderStatusRepo)
                if closeStatus == OrderPlacementStatus.Success then verify(orderRepo).save(placedExitOrder)
                else verify(orderRepo, never).save(placedExitOrder)
                verify(orderRepo, never).save(placedOpenOrder)
                disp.submittedActions mustBe List(Action.ProcessTradeOrderPlacement(placedExitOrder))
              }
            }
          }
        }

        s"recover the filled $targetPosition entry after its reversal from $initialPosition cannot be recorded" in {
          val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
          val openRule        = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
          val settings        = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
          val tradeState      = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
          val exitOrder       = TradeOrder.Exit(state.currencyPair, Markets.priceRange.close)
          val placedExitOrder = TradeOrderPlacement(Users.uid, exitOrder, settings.broker, now, List(exitExecution))
          val openOrder       = settings.trading.toOrder(targetPosition, state.currencyPair, Markets.priceRange.close)
          val openedOrder     = Trades.openedOrder.copy(position = targetPosition, volume = settings.trading.volume)
          val placedOpenOrder =
            TradeOrderPlacement(Users.uid, openOrder, settings.broker, now, List(Trades.execution), Some(BrokerPosition.from(openedOrder)))
          val recordingError = new RuntimeException("Entry could not be saved")
          when(settRepo.get(any[UserId])).thenReturnIO(settings)
          when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
          when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]]))
            .thenReturn(IO.pure(Nil), IO.pure(List(openedOrder)))
          when(brokerClient.findEntryExecutions(any[BrokerParameters], any[CurrencyPair], any[TradeOrder.Position]))
            .thenReturnIO(List(Trades.execution))
          when(brokerClient.submit(settings.broker, exitOrder)).thenReturnIO(exitFill)
          when(brokerClient.submit(settings.broker, openOrder)).thenReturnIO(entryFill)
          when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
          when(orderRepo.save(placedExitOrder)).thenReturnUnit
          when(orderRepo.save(placedOpenOrder)).thenReturn(IO.raiseError(recordingError), IO.unit)

          val result = for
            svc         <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
            firstResult <- svc.processMarketStateUpdate(tradeState).attempt
            exitActions <- IO(disp.submittedActions.toList)
            _           <- svc.processMarketStateUpdate(tradeState.copy(currentPosition = None))
          yield (firstResult, exitActions)

          result.asserting { case (firstResult, exitActions) =>
            firstResult mustBe Left(recordingError)
            exitActions mustBe List(Action.ProcessTradeOrderPlacement(placedExitOrder))
            // Before the entry, after its fill, and again when the retry finds the position open
            verify(brokerClient, times(3)).find(settings.broker, NonEmptyList.one(state.currencyPair))
            verify(brokerClient).submit(settings.broker, exitOrder)
            verify(brokerClient).submit(settings.broker, openOrder)
            verify(brokerClient).findEntryExecutions(settings.broker, state.currencyPair, targetPosition)
            verifyNoMoreInteractions(brokerClient)
            verify(orderStatusRepo).save(placedExitOrder, OrderPlacementStatus.Success)
            verify(orderStatusRepo, times(2)).save(placedOpenOrder, OrderPlacementStatus.Success)
            verify(orderRepo).save(placedExitOrder)
            verify(orderRepo, times(2)).save(placedOpenOrder)
            disp.submittedActions mustBe List(
              Action.ProcessTradeOrderPlacement(placedExitOrder),
              Action.ProcessTradeOrderPlacement(placedOpenOrder)
            )
          }
        }

        List[(String, IO[List[OrderExecution]], List[OrderExecution])](
          ("with its executions", IO.pure(List(Trades.execution)), List(Trades.execution)),
          ("without executions", IO.pure(Nil), Nil),
          ("when its executions cannot be retrieved", IO.raiseError(AppError.ClientFailure("oanda", "get-transaction returned 401")), Nil)
        ).foreach { case (description, lookup, executions) =>
          s"record an already open $targetPosition position $description instead of flipping from $initialPosition again" in {
            val (settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp) = mocks
            val openRule    = Rule(openAction, Rule.Condition.TrendIs(Direction.Upward))
            val settings    = Settings.trade.copy(strategy = TradeStrategy(List(openRule), Nil))
            val tradeState  = state.copy(currentPosition = state.currentPosition.map(_.copy(position = initialPosition)))
            val openedOrder = Trades.openedOrder.copy(
              currencyPair = state.currencyPair,
              position = targetPosition,
              openPrice = BigDecimal("2.5"),
              volume = settings.trading.volume * 2
            )
            val filledOrder     = TradeOrder.Enter(targetPosition, state.currencyPair, openedOrder.openPrice, openedOrder.volume)
            val placedOpenOrder =
              TradeOrderPlacement(Users.uid, filledOrder, settings.broker, now, executions, Some(BrokerPosition.Open(targetPosition, 2.5)))
            when(settRepo.get(any[UserId])).thenReturnIO(settings)
            when(dataClient.latestPrice(any[CurrencyPair])).thenReturnIO(Markets.priceRange)
            when(brokerClient.find(any[BrokerParameters], any[NonEmptyList[CurrencyPair]])).thenReturnIO(List(openedOrder))
            when(brokerClient.findEntryExecutions(any[BrokerParameters], any[CurrencyPair], any[TradeOrder.Position])).thenReturn(lookup)
            when(orderStatusRepo.save(any[TradeOrderPlacement], any[OrderPlacementStatus])).thenReturnUnit
            when(orderRepo.save(any[TradeOrderPlacement])).thenReturnUnit

            val result = for
              svc <- TradeService.make[IO](settRepo, orderRepo, orderStatusRepo, brokerClient, dataClient, disp)
              _   <- svc.processMarketStateUpdate(tradeState)
            yield ()

            result.asserting { _ =>
              verify(brokerClient).find(settings.broker, NonEmptyList.one(state.currencyPair))
              verify(brokerClient).findEntryExecutions(settings.broker, state.currencyPair, targetPosition)
              verifyNoMoreInteractions(brokerClient)
              verify(orderStatusRepo).save(placedOpenOrder, OrderPlacementStatus.Success)
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
