package currexx.clients.messenger.ntfy

import cats.effect.{IO, Ref}
import currexx.domain.errors.AppError
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.StringBody
import sttp.client4.testing.ResponseStub
import sttp.model.{MediaType, StatusCode}

import java.util.concurrent.TimeoutException
import java.util.concurrent.atomic.AtomicInteger
import scala.concurrent.duration.*

class NtfyClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val config = NtfyConfig(enabled = true, "https://ntfy.example.com", "currexx-logs")

  "An NtfyClient" should {
    "publish a UTF-8 message with its title to the configured topic" in {
      val message        = "Price changed: £1.25 → €1.45"
      val testingBackend = fs2BackendStub.whenRequestMatchesPartial {
        case r
            if r.isPost && r.uri.toString == "https://ntfy.example.com/currexx-logs" &&
              r.hasHeader("Title", "Currexx Warn") && r.hasContentType(MediaType.TextPlain.charset("UTF-8")) &&
              r.body == StringBody(message, "utf-8", MediaType.TextPlain) && r.options.readTimeout == 5.seconds =>
          ResponseStub.adjust("published", StatusCode.Ok)
        case _ => throw new IllegalArgumentException("Unexpected ntfy request")
      }

      NtfyClient.make[IO](config, testingBackend).flatMap(_.send("Currexx Warn", message)).assertVoid
    }

    "accept an empty successful response" in {
      val testingBackend = fs2BackendStub.whenAnyRequest.thenRespondWithCode(StatusCode.NoContent)

      NtfyClient.make[IO](config, testingBackend).flatMap(_.send("Currexx Error", "Failure")).assertVoid
    }

    "report an unsuccessful response without retrying" in {
      val requests       = AtomicInteger(0)
      val testingBackend = fs2BackendStub.whenAnyRequest.thenRespond {
        requests.incrementAndGet()
        ResponseStub.adjust("Service unavailable", StatusCode.ServiceUnavailable)
      }

      NtfyClient.make[IO](config, testingBackend).flatMap(_.send("Currexx Error", "Failure")).attempt.asserting { result =>
        result mustBe Left(AppError.ClientFailure("ntfy", "publish returned 503: Service unavailable"))
        requests.get mustBe 1
      }
    }

    "propagate transport failures without retrying" in {
      val requests       = AtomicInteger(0)
      val error          = new IllegalStateException("Connection unavailable")
      val testingBackend = fs2BackendStub.whenAnyRequest.thenRespondF {
        IO.delay(requests.incrementAndGet()).flatMap(_ => IO.raiseError(error))
      }

      NtfyClient.make[IO](config, testingBackend).flatMap(_.send("Currexx Error", "Failure")).attempt.asserting { result =>
        result mustBe Left(error)
        requests.get mustBe 1
      }
    }

    "cancel a stalled publish after five seconds" in {
      val requests = AtomicInteger(0)
      val result   = for
        canceled <- Ref.of[IO, Boolean](false)
        backend = fs2BackendStub.whenAnyRequest.thenRespondF {
          IO.delay(requests.incrementAndGet()).flatMap(_ => IO.never).onCancel(canceled.set(true))
        }
        client      <- NtfyClient.make[IO](config, backend)
        response    <- client.send("Currexx Warn", "Failure").attempt.timeout(7.seconds)
        wasCanceled <- canceled.get
      yield (response, wasCanceled)

      result.asserting { case (response, wasCanceled) =>
        response.left.toOption.get mustBe a[TimeoutException]
        wasCanceled mustBe true
        requests.get mustBe 1
      }
    }
  }

  "NtfyClient creation" should
    List("", " \t\n").foreach { topic =>
      s"reject a blank topic of length ${topic.length} when enabled" in
        NtfyClient.make[IO](config.copy(topic = topic), fs2BackendStub).attempt.asserting {
          case Left(error) =>
            error mustBe a[IllegalArgumentException]
            error.getMessage mustBe "ntfy topic must be nonblank when enabled"
          case Right(_) => fail("Expected client creation to reject a blank topic")
        }
    }
}
