package currexx.core.common.action

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.core.fixtures.Signals
import currexx.core.market.MarketService
import currexx.core.monitor.MonitorService
import currexx.core.signal.SignalService
import currexx.core.trade.TradeService

import scala.concurrent.duration.*

class ActionProcessorSpec extends CatsSpec {

  "An ActionProcessor" should {
    "process submitted signals" in {
      val (monsvc, sigsvc, marksvc, tradesvc) = mocks
      val result = for {
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc)
        _          <- dispatcher.dispatch(Action.ProcessSignal(Signals.macd))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      } yield res

      result.unsafeToFuture().map { r =>
        r mustBe ()
      }
    }
  }

  def mocks: (MonitorService[IO], SignalService[IO], MarketService[IO], TradeService[IO]) =
    (mock[MonitorService[IO]], mock[SignalService[IO]], mock[MarketService[IO]], mock[TradeService[IO]])
}
