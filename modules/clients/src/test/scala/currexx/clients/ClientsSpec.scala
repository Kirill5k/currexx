package currexx.clients

import cats.effect.IO
import currexx.clients.broker.oanda.OandaBrokerConfig
import currexx.clients.data.alphavantage.AlphaVantageConfig
import currexx.clients.data.oanda.OandaDataConfig
import currexx.clients.data.twelvedata.TwelveDataConfig
import currexx.clients.messenger.ntfy.NtfyConfig
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.StringBody
import sttp.client4.testing.ResponseStub
import sttp.model.{MediaType, StatusCode}

class ClientsSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val config = ClientsConfig(
    AlphaVantageConfig("https://alphavantage.example.com", "api-key"),
    TwelveDataConfig("https://twelvedata.example.com", "api-key"),
    OandaBrokerConfig("https://demo.oanda.example.com", "https://live.oanda.example.com"),
    OandaDataConfig("https://oanda.example.com", "api-key"),
    NtfyConfig(enabled = true, "https://ntfy.example.com", "currexx-logs")
  )

  "Clients" should {
    "send messages through ntfy when enabled" in {
      val testingBackend = fs2BackendStub.whenRequestMatchesPartial {
        case r
            if r.isPost && r.uri.toString == "https://ntfy.example.com/currexx-logs" &&
              r.hasHeader("Title", "Currexx Warn") && r.body == StringBody("Failure", "utf-8", MediaType.TextPlain) =>
          ResponseStub.adjust("published", StatusCode.Ok)
        case _ => throw new IllegalArgumentException("Unexpected ntfy request")
      }

      Clients.make[IO](config, testingBackend).flatMap(_.messenger.send("Currexx Warn", "Failure")).assertVoid
    }

    "send no requests when ntfy is disabled even without a URI or topic" in {
      val testingBackend = fs2BackendStub.whenAnyRequest.thenThrow(new IllegalStateException("Must not send"))

      Clients
        .make[IO](config.copy(ntfy = NtfyConfig(enabled = false, "", "")), testingBackend)
        .flatMap(_.messenger.send("Currexx Error", "Failure"))
        .assertVoid
    }

    "skip ntfy validation when disabled" in {
      val testingBackend = fs2BackendStub.whenAnyRequest.thenThrow(new IllegalStateException("Must not send"))

      Clients
        .make[IO](config.copy(ntfy = NtfyConfig(enabled = false, "invalid URI", " ")), testingBackend)
        .flatMap(_.messenger.send("Currexx Error", "Failure"))
        .assertVoid
    }
  }
}
