package currexx.core.common.action

import currexx.core.fixtures.Processing
import currexx.core.signal.SignalBatch
import cats.data.NonEmptyList
import cats.effect.{Deferred, IO}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.market.MarketService
import currexx.core.monitor.MonitorService
import currexx.core.signal.SignalService
import currexx.core.trade.{BrokerPosition, TradeService}
import currexx.core.settings.SettingsService
import currexx.domain.market.{CandleCount, CurrencyPair, Interval}
import currexx.domain.signal.Direction
import currexx.domain.user.UserId
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class ActionProcessorSpec extends IOWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "An ActionProcessor" should {
    "forward the configured candle count when fetching market data" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      val count                                       = CandleCount.from(250).toOption.get
      val pairs                                       = NonEmptyList.of(Markets.gbpeur)

      val result = for
        processed <- Deferred[IO, Unit]
        _ = when(tradesvc.fetchMarketData(Users.uid, pairs, Interval.H1, count)).thenReturn(processed.complete(()).void)
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.FetchMarketData(Users.uid, pairs, Interval.H1, count))
        _          <- processor.run.interruptWhen(processed.get.attempt).compile.drain.timeout(3.seconds)
      yield ()

      result.asserting { _ =>
        verify(tradesvc).fetchMarketData(Users.uid, pairs, Interval.H1, count)
        verifyNoMoreInteractions(tradesvc)
        succeed
      }
    }

    "process detected signal batches" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks

      when(marksvc.processSignals(any[UserId], any[CurrencyPair], any[SignalBatch])).thenReturn(IO.unit)

      val signal = Signals.trend(Direction.Upward)
      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal))))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).processSignals(Users.uid, Markets.gbpeur, Processing.batch(List(signal)))
        r mustBe ()
      }
    }

    "read and pass the current state to trade evaluation when its entry price is known" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      val state                                       = Markets.state.copy(currentPosition = None)
      when(marksvc.getState(Users.uid, Markets.gbpeur)).thenReturnIO(state)
      when(tradesvc.processMarketStateUpdate(state)).thenReturnUnit
      (for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        _          <- processor.run.interruptAfter(2.second).compile.drain
      yield ()).asserting { _ =>
        verify(marksvc).getState(Users.uid, Markets.gbpeur)
        verify(tradesvc).processMarketStateUpdate(state)
        verifyNoMoreInteractions(tradesvc)
        succeed
      }
    }

    "enrich a missing entry price before passing the repaired state to trade evaluation" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks
      val snapshot                                    = Markets.state
      val position                                    = BrokerPosition.Open(snapshot.currentPosition.get.position, BigDecimal("1.25"))
      val repaired = snapshot.copy(currentPosition = snapshot.currentPosition.map(_.copy(openPrice = Some(BigDecimal("1.25")))))
      when(marksvc.getState(Users.uid, Markets.gbpeur)).thenReturnIO(snapshot)
      when(tradesvc.findBrokerPosition(Users.uid, Markets.gbpeur)).thenReturnIO(position)
      when(marksvc.reconcileEntryPrice(snapshot, position)).thenReturnIO(repaired)
      when(tradesvc.processMarketStateUpdate(repaired)).thenReturnUnit
      (for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessMarketStateUpdate(Users.uid, Markets.gbpeur))
        _          <- processor.run.interruptAfter(2.second).compile.drain
      yield ()).asserting { _ =>
        verify(marksvc).reconcileEntryPrice(snapshot, position)
        verify(tradesvc).processMarketStateUpdate(repaired)
        succeed
      }
    }

  }

  def mocks: (MonitorService[IO], SignalService[IO], MarketService[IO], TradeService[IO], SettingsService[IO]) =
    (mock[MonitorService[IO]], mock[SignalService[IO]], mock[MarketService[IO]], mock[TradeService[IO]], mock[SettingsService[IO]])
}
