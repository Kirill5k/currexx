package currexx.core.common.action

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.IOWordSpec
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.market.MarketService
import currexx.core.monitor.MonitorService
import currexx.core.signal.{Signal, SignalService}
import currexx.core.trade.TradeService
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId

import scala.concurrent.duration.*

class ActionProcessorSpec extends IOWordSpec {

  "An ActionProcessor" should {
    "process submitted signals" in {
      val (monsvc, sigsvc, marksvc, tradesvc) = mocks

      when(marksvc.processSignals(any[UserId], any[CurrencyPair], anyList[Signal])).thenReturn(IO.unit)

      val result = for {
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc)
        _          <- dispatcher.dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged)))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      } yield res

      result.unsafeToFuture().map { r =>
        verify(marksvc).processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        r mustBe ()
      }
    }
  }

  def mocks: (MonitorService[IO], SignalService[IO], MarketService[IO], TradeService[IO]) =
    (mock[MonitorService[IO]], mock[SignalService[IO]], mock[MarketService[IO]], mock[TradeService[IO]])
}
