package currexx.clients.broker.xtb

import cats.effect.IO
import cats.data.NonEmptyList
import currexx.clients.ClientSpec
import currexx.clients.broker.BrokerParameters
import currexx.clients.broker.vindaloo.{VindalooClient, VindalooConfig}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import currexx.domain.market.Currency.{CAD, EUR}
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.asynchttpclient.fs2.AsyncHttpClientFs2Backend
import sttp.client3.testing.SttpBackendStub
import sttp.client3.{Response, SttpBackend, SttpBackendOptions}
import sttp.model.StatusCode
import sttp.ws.{WebSocket, WebSocketFrame}
import sttp.ws.testing.WebSocketStub

import scala.concurrent.duration.*

class XtbClientSpec extends ClientSpec {
  val config = XtbConfig("wss://ws.xtb.com")
  val pair   = CurrencyPair(EUR, CAD)
  val price  = BigDecimal(1.341)

  val brokerConfig: BrokerParameters.Xtb = BrokerParameters.Xtb("foo", "bar", true)

  "A XtbClient" should {
    "return error on failed authentication" ignore {
      val testingBackend: SttpBackend[IO, Fs2Streams[IO] with WebSockets] = backendStub.whenAnyRequest
        .thenRespond(
          SttpBackendStub.RawStream(
            WebSocketStub
              .initialReceive(List(WebSocketFrame.text("Hello, World!")))
              .thenRespond(_ => List(WebSocketFrame.text("Hello, World!")))
          )
        )

      val result = for
        client <- XtbClient.make[IO](config, testingBackend)
        res    <- client.submit(brokerConfig, TradeOrder.Exit(pair, price))
      yield res

      result.assertError(AppError.AccessDenied("foo"))
    }

    "send enter market request" ignore {
      val result = AsyncHttpClientFs2Backend
        .resource[IO](SttpBackendOptions(connectionTimeout = 3.minutes, proxy = None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            order = TradeOrder.Enter(TradeOrder.Position.Buy, pair, price, BigDecimal(0.1))
            res <- client.submit(brokerConfig, order)
          yield res
        }

      result.asserting(_ mustBe ())
    }

    "get existing order" ignore {
      val result = AsyncHttpClientFs2Backend
        .resource[IO](SttpBackendOptions(connectionTimeout = 3.minutes, proxy = None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            res    <- client.getCurrentOrders(brokerConfig, NonEmptyList.of(pair))
          yield res
        }

      result.asserting { order =>
        order.map(_.currencyPair) mustBe Some(pair)
      }
    }

    "send exit market request" ignore {
      val result = AsyncHttpClientFs2Backend
        .resource[IO](SttpBackendOptions(connectionTimeout = 3.minutes, proxy = None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            _      <- IO.sleep(10.seconds)
            res    <- client.submit(brokerConfig, TradeOrder.Exit(pair, price))
          yield res
        }

      result.asserting(_ mustBe ())
    }
  }
}
