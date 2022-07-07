package currexx.clients.broker.xtb

import cats.effect.IO
import currexx.clients.ApiClientSpec
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.vindaloo.{VindalooClient, VindalooConfig}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import squants.market.{GBP, USD}
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.{Response, SttpBackend}
import sttp.model.StatusCode
import sttp.ws.{WebSocket, WebSocketFrame}
import sttp.ws.testing.WebSocketStub

class XtbClientSpec extends ApiClientSpec {

  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = XtbConfig("wss://echo.websocket.org")
  val pair   = CurrencyPair(GBP, USD)

  "A XtbClient" should {
    "return error on failed authentication" in {
      val testingBackend: SttpBackend[IO, Fs2Streams[IO] with WebSockets] = backendStub
        .whenRequestMatches(_ => true)
        .thenRespond(
          WebSocketStub
            .noInitialReceive
            .thenRespond(_ => List(WebSocketFrame.text(json("xtb/login-failure-response.json")))),
          StatusCode.SwitchingProtocols
        )

      val result = for
        client <- XtbClient.make[IO](config, testingBackend)
        res    <- client.submit(BrokerParameters.Xtb("foo", "bar", true), pair, TradeOrder.Exit)
      yield res

      result.assertError(AppError.AccessDenied("foo"))
    }

    "send enter market requests" in {
      pending
    }

    "send exit market requests" in {
      pending
    }
  }
}
