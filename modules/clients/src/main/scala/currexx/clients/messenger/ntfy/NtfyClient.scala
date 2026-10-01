package currexx.clients.messenger.ntfy

import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Fs2HttpClient
import currexx.clients.messenger.MessengerClient
import currexx.domain.errors.AppError
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.model.MediaType

import scala.concurrent.duration.*

final private class NtfyClient[F[_]](
    private val config: NtfyConfig,
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends MessengerClient[F] with Fs2HttpClient[F] {

  override protected val name: String = "ntfy"

  override def send(title: String, message: String): F[Unit] =
    dispatch {
      basicRequest
        .post(uri"${config.baseUri}/${config.topic}")
        .header("Title", title)
        .contentType(MediaType.TextPlain.charset("UTF-8"))
        .body(message)
        .readTimeout(5.seconds)
    }.flatMap { response =>
      response.body match
        case Right(_)    => F.unit
        case Left(error) => F.raiseError(AppError.ClientFailure(name, s"publish returned ${response.code.code}: $error"))
    }
}

private[clients] object NtfyClient:
  def make[F[_]: {Temporal, Logger}](
      config: NtfyConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[MessengerClient[F]] =
    Temporal[F]
      .raiseWhen(config.topic.isBlank)(new IllegalArgumentException("ntfy topic must be nonblank when enabled"))
      .as(NtfyClient[F](config, backend))
