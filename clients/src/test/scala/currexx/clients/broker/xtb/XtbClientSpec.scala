package currexx.clients.broker.xtb

import cats.effect.IO
import currexx.clients.ApiClientSpec
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.vindaloo.{VindalooClient, VindalooConfig}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import squants.market.{GBP, USD}
import sttp.client3.{Response, SttpBackend}

class XtbClientSpec extends ApiClientSpec {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = XtbConfig("wss://echo.websocket.org")
  val pair   = CurrencyPair(GBP, USD)

  "A XtbClient" should {
    "return error on failed authentication" in {
      pending
    }
    
    "send enter market requests" in {
      pending
    }

    "send exit market requests" in {
      pending
    }
  }
}
