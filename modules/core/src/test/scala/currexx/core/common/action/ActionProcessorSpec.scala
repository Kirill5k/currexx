package currexx.core.common.action

import cats.effect.IO
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.market.MarketService
import currexx.core.monitor.MonitorService
import currexx.core.signal.{Signal, SignalService}
import currexx.core.trade.TradeService
import currexx.core.settings.SettingsService
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import kirill5k.common.cats.test.IOWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration.*

class ActionProcessorSpec extends IOWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  "An ActionProcessor" should {
    "process submitted signals" in {
      val (monsvc, sigsvc, marksvc, tradesvc, settvc) = mocks

      when(marksvc.processSignals(any[UserId], any[CurrencyPair], anyList[Signal])).thenReturn(IO.unit)

      val result = for
        dispatcher <- ActionDispatcher.make[IO]
        processor  <- ActionProcessor.make[IO](dispatcher, monsvc, sigsvc, marksvc, tradesvc, settvc)
        _          <- dispatcher.dispatch(Action.ProcessSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged)))
        res        <- processor.run.interruptAfter(2.second).compile.drain
      yield res

      result.asserting { r =>
        verify(marksvc).processSignals(Users.uid, Markets.gbpeur, List(Signals.trendDirectionChanged))
        r mustBe ()
      }
    }
  }

  def mocks: (MonitorService[IO], SignalService[IO], MarketService[IO], TradeService[IO], SettingsService[IO]) =
    (mock[MonitorService[IO]], mock[SignalService[IO]], mock[MarketService[IO]], mock[TradeService[IO]], mock[SettingsService[IO]])
}
