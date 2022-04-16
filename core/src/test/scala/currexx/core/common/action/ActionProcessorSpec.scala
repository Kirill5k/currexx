package currexx.core.common.action

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.core.fixtures.Signals
import currexx.core.monitor.MonitorService

import scala.concurrent.duration.*

class ActionProcessorSpec extends CatsSpec {

  "An ActionProcessor" should {
    "process submitted signals" in {
      val result = for {
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, mock[MonitorService[IO]])
        _          <- dispatcher.dispatch(Action.SignalSubmitted(Signals.macd))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      } yield res

      result.unsafeToFuture().map { r =>
        r mustBe ()
      }
    }
  }
}
