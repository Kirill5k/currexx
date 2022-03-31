package currexx.core

import cats.effect.{IO, IOApp}
import currexx.core.common.config.AppConfig
import currexx.core.health.Health
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object Application extends IOApp.Simple:
  given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  override val run: IO[Unit] =
    for
      config <- AppConfig.load[IO]
      health <- Health.make[IO]
      http   <- Http.make[IO](health)
      _      <- Server.serve[IO](config.server, http.app, runtime.compute).compile.drain
    yield ()
