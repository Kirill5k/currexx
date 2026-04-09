package currexx.core.health

import cats.effect.Async
import cats.syntax.functor.*
import currexx.core.common.http.Controller

final class Health[F[_]] private (
    val controller: Controller[F]
)

object Health:
  def make[F[_]: Async]: F[Health[F]] =
    HealthController.make[F].map(hc => Health[F](hc))
