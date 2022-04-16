package currexx.clients

import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.applicativeError.*
import org.typelevel.log4cats.Logger
import sttp.client3.{Request, Response, SttpBackend}

import scala.concurrent.duration.*

final case class HttpClientConfig(
    baseUri: String,
    apiKey: String
)

trait HttpClient[F[_]] {
  protected val name: String
  protected val backend: SttpBackend[F, Any]

  protected val delayBetweenFailures: FiniteDuration = 10.seconds

  protected def dispatch[T](request: Request[T, Any])(using F: Temporal[F], logger: Logger[F]): F[Response[T]] =
    dispatchWithRetry(request)

  private def dispatchWithRetry[T](request: Request[T, Any], attempt: Int = 0)(using F: Temporal[F], logger: Logger[F]): F[Response[T]] =
    backend
      .send(request)
      .handleErrorWith { error =>
        val cause      = Option(error.getCause)
        val errorClass = cause.fold(error.getClass.getSimpleName)(_.getClass.getSimpleName)
        val errorMsg   = cause.fold(error.getMessage)(_.getMessage)
        val message    = s"$name-client/${errorClass.toLowerCase}-$attempt: ${errorMsg}\n$error"
        (if (attempt >= 50 && attempt % 10 == 0) logger.error(message) else logger.warn(message)) *>
          F.sleep(delayBetweenFailures) *> dispatchWithRetry(request, attempt + 1)
      }
}