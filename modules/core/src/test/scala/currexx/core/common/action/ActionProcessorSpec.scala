package currexx.core.common.action

import cats.effect.IO
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.market.{MarketService, MarketState}
import currexx.core.monitor.MonitorService
import currexx.core.signal.{Signal, SignalService}
import currexx.core.trade.{BrokerPosition, TradeService}
import currexx.core.settings.SettingsService
import currexx.domain.market.{CurrencyPair, TradeOrder}
import currexx.domain.signal.Direction
import currexx.domain.user.UserId
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class ActionProcessorSpec extends IOWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "An ActionProcessor" should {
    "process manual signals separately from detected candle batches" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      when(marksvc.processManualSignal(any[Signal])).thenReturn(IO.unit)

      val signal = Signals.trend(Direction.Upward)
      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessManualSignal(signal))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).processManualSignal(signal)
        verifyNoMoreInteractions(marksvc)
        r mustBe ()
      }
    }

    "process submitted signals" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks

      when(marksvc.processSignals(any[UserId], any[CurrencyPair], anyList[Signal])).thenReturn(IO.unit)

      val signal = Signals.trend(Direction.Upward)
      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(signal)))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).processSignals(Users.uid, Markets.gbpeur, List(signal))
        r mustBe ()
      }
    }

    "read the entry price from the broker before evaluating rules for a position held without one" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      val brokerPosition                              = BrokerPosition.Open(TradeOrder.Position.Buy, BigDecimal("1.1002"))
      val repaired = Markets.state.copy(currentPosition = Some(Markets.positionState.copy(openPrice = Some(BigDecimal("1.1002")))))
      when(marksvc.getState(any[UserId], any[CurrencyPair])).thenReturnIO(Markets.state)
      when(tradesvc.findBrokerPosition(any[UserId], any[CurrencyPair])).thenReturnIO(brokerPosition)
      when(marksvc.reconcileEntryPrice(any[MarketState], any[BrokerPosition])).thenReturnIO(repaired)
      when(tradesvc.processMarketStateUpdate(any[MarketState])).thenReturn(IO.unit)

      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).getState(Users.uid, Markets.gbpeur)
        verify(tradesvc).findBrokerPosition(Users.uid, Markets.gbpeur)
        verify(marksvc).reconcileEntryPrice(Markets.state, brokerPosition)
        verify(tradesvc).processMarketStateUpdate(repaired)
        r mustBe ()
      }
    }

    "evaluate rules without reading the broker when the entry price is known" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      val priced = Markets.state.copy(currentPosition = Some(Markets.positionState.copy(openPrice = Some(BigDecimal("1.1002")))))
      when(marksvc.getState(any[UserId], any[CurrencyPair])).thenReturnIO(priced)
      when(tradesvc.processMarketStateUpdate(any[MarketState])).thenReturn(IO.unit)

      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).getState(Users.uid, Markets.gbpeur)
        verify(tradesvc).processMarketStateUpdate(priced)
        verifyNoMoreInteractions(marksvc, tradesvc)
        r mustBe ()
      }
    }
  }

  def mocks: (MonitorService[IO], SignalService[IO], MarketService[IO], TradeService[IO], SettingsService[IO]) =
    (mock[MonitorService[IO]], mock[SignalService[IO]], mock[MarketService[IO]], mock[TradeService[IO]], mock[SettingsService[IO]])
}
