package currexx.core

import cats.effect.{IO, IOApp}
import currexx.clients.Clients
import currexx.core.auth.Auth
import currexx.core.common.action.{Action, ActionDispatcher, ActionProcessor}
import currexx.core.common.config.AppConfig
import currexx.core.common.http.Http
import currexx.core.common.logging.Logger
import currexx.core.health.Health
import currexx.core.market.Markets
import currexx.core.signal.Signals
import currexx.core.monitor.Monitors

object Application extends IOApp.Simple:
  override val run: IO[Unit] =
    Logger.make[IO].flatMap { implicit logger =>
      for
        config <- AppConfig.load[IO]
        _ <- Resources.make[IO](config).use { res =>
          for
            dispatcher <- ActionDispatcher.make[IO].flatTap(_.dispatch(Action.RescheduleAllMonitors))
            clients    <- Clients.make[IO](config.clients, res.sttpBackend)
            health     <- Health.make[IO]
            auth       <- Auth.make(config.auth, res.mongo)
            signals    <- Signals.make(res.mongo, dispatcher)
            monitors   <- Monitors.make(res.mongo, clients, dispatcher)
            markets    <- Markets.make(res.mongo, clients, dispatcher)
            http       <- Http.make[IO](health, auth, signals, monitors, markets)
            processor  <- ActionProcessor.make[IO](dispatcher, monitors.service, signals.service, markets.service)
            _ <- Server
              .serve[IO](config.server, http.app, runtime.compute)
              .concurrently(processor.run)
              .compile
              .drain
          yield ()
        }
      yield ()
    }
