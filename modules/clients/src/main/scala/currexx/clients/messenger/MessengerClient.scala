package currexx.clients.messenger

import cats.Monad
import cats.syntax.applicative.*

trait MessengerClient[F[_]]:
  def send(title: String, message: String): F[Unit]

final private class LiveMessengerClient[F[_]](
    private val ntfyClient: MessengerClient[F]
) extends MessengerClient[F]:
  override def send(title: String, message: String): F[Unit] =
    ntfyClient.send(title, message)

object MessengerClient:
  def noop[F[_]: Monad]: MessengerClient[F] =
    new MessengerClient[F] {
      override def send(title: String, message: String): F[Unit] = Monad[F].unit
    }

  def make[F[_]: Monad](ntfyClient: MessengerClient[F]): F[MessengerClient[F]] =
    LiveMessengerClient[F](ntfyClient).pure[F]
