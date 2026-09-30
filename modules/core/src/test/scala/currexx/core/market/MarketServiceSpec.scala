package currexx.core.market

import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.domain.user.UserId
import currexx.core.common.action.Action
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Signals, Trades, Users}
import currexx.core.trade.BrokerPosition
import currexx.domain.market.{CurrencyPair, OrderExecution, TradeOrder}
import currexx.domain.signal.{Condition, Direction, ValueRole}
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec
import kirill5k.common.syntax.time.*
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class MarketServiceSpec extends IOWordSpec {
  given Logger[IO] = Slf4jLogger.getLogger[IO]
  given Clock[IO]  = Clock.mock[IO](Markets.ts)

  "A MarketService" when {
    "clearState" should {
      "delete all existing market states and close orders" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.deleteAll(any[UserId])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.clearState(Users.uid, true)
        yield state

        result.asserting { res =>
          verify(stateRepo).deleteAll(Users.uid)
          disp.submittedActions mustBe List(Action.CloseAllOpenOrders(Users.uid))
          res mustBe ()
        }
      }

      "delete all existing market states without closing orders" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.deleteAll(any[UserId])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.clearState(Users.uid, false)
        yield state

        result.asserting { res =>
          verify(stateRepo).deleteAll(Users.uid)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }

      "delete existing market for a single currency" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.delete(any[UserId], any[CurrencyPair])).thenReturnUnit

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.clearState(Users.uid, Markets.gbpeur, true)
        yield state

        result.asserting { res =>
          verify(stateRepo).delete(Users.uid, Markets.gbpeur)
          disp.submittedActions mustBe List(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          res mustBe ()
        }
      }
    }

    "getState" should {
      "return state of all traded currencies" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.getAll(any[UserId])).thenReturnIO(List(Markets.state))

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.getState(Users.uid)
        yield state

        result.asserting { res =>
          verify(stateRepo).getAll(Users.uid)
          disp.submittedActions mustBe empty
          res mustBe List(Markets.state)
        }
      }
    }

    "processTradeOrderPlacement" should {
      val openedAt  = Markets.ts.minusSeconds(600)
      val filledAt  = Trades.execution.time
      val longAt    = (price: String) => Some(PositionState(TradeOrder.Position.Buy, openedAt, Some(BigDecimal(price))))
      val shortAt   = (price: String) => Some(PositionState(TradeOrder.Position.Sell, openedAt, Some(BigDecimal(price))))
      val long      = (price: String) => Some(BrokerPosition.Open(TradeOrder.Position.Buy, BigDecimal(price)))
      val short     = (price: String) => Some(BrokerPosition.Open(TradeOrder.Position.Sell, BigDecimal(price)))
      val execution = List(Trades.execution)

      def placeEntry(current: Option[PositionState], executions: List[OrderExecution], brokerPosition: Option[BrokerPosition]) = {
        val (stateRepo, disp) = mocks
        val state             = Markets.state.copy(currentPosition = current)
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processTradeOrderPlacement(Trades.order.copy(executions = executions, brokerPosition = brokerPosition))
        yield ()
        result.map { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          disp.submittedActions mustBe empty
          (stateRepo, state)
        }
      }

      List[(String, Option[PositionState], List[OrderExecution], Option[BrokerPosition], Option[PositionState])](
        (
          "open the position at the broker's entry price and the time it filled",
          None,
          execution,
          long("1.15"),
          Some(PositionState(TradeOrder.Position.Buy, filledAt, Some(BigDecimal("1.15"))))
        ),
        (
          "open a position without an entry price when the broker's position is unknown after the fill",
          None,
          execution,
          Some(BrokerPosition.Unknown),
          Some(PositionState(TradeOrder.Position.Buy, filledAt))
        ),
        (
          "open a position without an entry price while its entry is pending",
          None,
          Nil,
          None,
          Some(PositionState(TradeOrder.Position.Buy, Trades.ts))
        ),
        (
          "take the broker's entry price after adding to a position, keeping when it opened",
          longAt("1.10"),
          execution,
          long("1.15"),
          longAt("1.15")
        ),
        (
          "drop the entry price after adding to a position when the broker's position is unknown",
          longAt("1.10"),
          Nil,
          Some(BrokerPosition.Unknown),
          Some(PositionState(TradeOrder.Position.Buy, openedAt))
        ),
        (
          "replace a position on the opposite side",
          shortAt("1.10"),
          execution,
          long("1.15"),
          Some(PositionState(TradeOrder.Position.Buy, filledAt, Some(BigDecimal("1.15"))))
        ),
        ("clear a position that the entry closed at the broker", shortAt("1.10"), execution, Some(BrokerPosition.Flat), None)
      ).foreach { case (description, current, executions, brokerPosition, expected) =>
        description in placeEntry(current, executions, brokerPosition).asserting { (stateRepo, state) =>
          verify(stateRepo).save(state.copy(currentPosition = expected))
          succeed
        }
      }

      List[(String, Option[PositionState], List[OrderExecution], Option[BrokerPosition])](
        ("a replayed fill", longAt("1.15"), execution, long("1.15")),
        ("an entry that only reduces the opposite side at the broker", shortAt("1.10"), execution, short("1.10")),
        ("an entry on the same side that is pending", longAt("1.10"), Nil, None)
      ).foreach { case (description, current, executions, brokerPosition) =>
        s"leave the position unchanged by $description" in placeEntry(current, executions, brokerPosition).asserting { (stateRepo, _) =>
          verifyNoMoreInteractions(stateRepo)
          succeed
        }
      }

      "clear the position on exit" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[Option[PositionState]])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processTradeOrderPlacement(Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, 1.3)))
        yield state

        result.asserting { res =>
          verify(stateRepo).update(Users.uid, Markets.gbpeur, None)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "processSignals" should {
      "save the updated state and evaluate rules when the profile changes" in {
        val signal            = Signals.trend(Direction.Downward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verify(stateRepo).save(Markets.state.applyCandleSignals(List(signal), signal.time).get)
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "save the candle watermark without evaluating rules when the profile is unchanged" in {
        val signal            = Signals.trend(Direction.Upward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(Markets.state.copy(lastCandleTime = Some(signal.time)))
          disp.submittedActions mustBe empty
        }
      }

      "skip older and repeated candles without saving" in {
        val candleTime        = Markets.ts.plus(2.hours)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state.copy(lastCandleTime = Some(candleTime)))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Downward, time = candleTime.minus(1.hour))))
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Downward, time = candleTime)))
        yield ()

        result.asserting { _ =>
          verify(stateRepo, times(2)).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }

      "create the state when it does not exist yet" in {
        val signal            = Signals.trend(Direction.Downward)
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnNone
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            MarketState.initial(Users.uid, Markets.gbpeur, Markets.ts).applyCandleSignals(List(signal), signal.time).get
          )
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "reload and recompute from the latest state after a version conflict" in {
        val olderTime  = Markets.ts.plus(1.hour)
        val candleTime = Markets.ts.plus(2.hours)
        val committed  =
          Markets.state.copy(profile = Markets.profile.copy(trend = Some(TrendState(Direction.Downward, olderTime))), version = Some(1))
        val signal = Signals.make(Users.uid, candleTime, Markets.gbpeur, Condition.ValueUpdated(ValueRole.Price, BigDecimal("1.25")))
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturn(IO.pure(Some(Markets.state)), IO.pure(Some(committed)))
        when(stateRepo.save(any[MarketState])).thenReturn(IO.pure(false), IO.pure(true))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, List(signal))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(Markets.state.applyCandleSignals(List(signal), candleTime).get)
          verify(stateRepo).save(committed.applyCandleSignals(List(signal), candleTime).get)
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "stop after a bounded number of conflicting attempts" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(false)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          res <- svc.processSignals(Users.uid, Markets.gbpeur, List(Signals.trend(Direction.Downward))).attempt
        yield res

        result.asserting { res =>
          res.left.map(_.getClass) mustBe Left(classOf[IllegalStateException])
          verify(stateRepo, times(5)).find(Users.uid, Markets.gbpeur)
          verify(stateRepo, times(5)).save(any[MarketState])
          disp.submittedActions mustBe empty
        }
      }

      "ignore empty batches" in {
        val (stateRepo, disp) = mocks
        val result            = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processSignals(Users.uid, Markets.gbpeur, Nil)
        yield ()

        result.asserting { _ =>
          verifyNoInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }
    }

    "processManualSignal" should {
      "save the updated profile without moving the candle watermark" in {
        val manualSignal      = Signals.trend(Direction.Downward, time = Markets.ts.minus(1.hour))
        val initialState      = Markets.state.copy(lastCandleTime = Some(Markets.ts))
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(initialState)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processManualSignal(manualSignal)
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            initialState.copy(
              profile = Markets.profile.copy(trend = Some(TrendState(Direction.Downward, manualSignal.time))),
              previousProfile = Some(Markets.profile)
            )
          )
          disp.submittedActions mustBe List(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        }
      }

      "not save anything when the signal leaves the profile unchanged" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.processManualSignal(Signals.trend(Direction.Upward))
        yield ()

        result.asserting { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }
    }

    "updateTimeState" should {
      val candleTime  = Markets.ts.plus(1.day)
      val dataWithGap = Markets.timeSeriesData.copy(prices = Markets.priceRanges.head.copy(time = candleTime) :: Markets.priceRanges)

      "shift the state when the gap between the latest two candles is greater than two intervals" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.updateTimeState(Users.uid, dataWithGap)
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(
            Markets.state.adjustForMarketClosure(1.day - 1.hour, candleTime).copy(lastTimeStateCandle = Some(candleTime))
          )
          disp.submittedActions mustBe empty
        }
      }

      "only record the candle when there is no significant gap" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state)
        when(stateRepo.save(any[MarketState])).thenReturnIO(true)

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.updateTimeState(Users.uid, Markets.timeSeriesData)
        yield ()

        result.asserting { _ =>
          verify(stateRepo).save(Markets.state.copy(lastTimeStateCandle = Some(Markets.timeSeriesData.latestTime)))
          disp.submittedActions mustBe empty
        }
      }

      "skip a gap that was already applied" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.find(any[UserId], any[CurrencyPair])).thenReturnSome(Markets.state.copy(lastTimeStateCandle = Some(candleTime)))

        val result = for
          svc <- MarketService.make[IO](stateRepo, disp)
          _   <- svc.updateTimeState(Users.uid, dataWithGap)
        yield ()

        result.asserting { _ =>
          verify(stateRepo).find(Users.uid, Markets.gbpeur)
          verifyNoMoreInteractions(stateRepo)
          disp.submittedActions mustBe empty
        }
      }
    }
  }

  def mocks: (MarketStateRepository[IO], MockActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], MockActionDispatcher[IO])
}
