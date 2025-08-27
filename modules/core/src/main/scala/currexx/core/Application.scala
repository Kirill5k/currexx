package currexx.core

import cats.effect.{IO, IOApp}
import cats.syntax.apply.*
import currexx.clients.Clients
import currexx.core.auth.Auth
import currexx.core.common.action.{Action, ActionDispatcher, ActionProcessor}
import currexx.core.common.config.{AppConfig, ServerConfig}
import currexx.core.common.http.Http
import currexx.core.common.logging.{LogEventProcessor, Logger}
import currexx.core.health.Health
import currexx.core.market.Markets
import currexx.core.signal.Signals
import currexx.core.trade.Trades
import currexx.core.monitor.Monitors
import currexx.core.settings.Settings
import kirill5k.common.cats.Clock
import kirill5k.common.http4s.Server
import fs2.Stream

object Application extends IOApp.Simple:
  given Conversion[ServerConfig, Server.Config] =
    (sc: ServerConfig) => Server.Config(sc.host, sc.port)

  override val run: IO[Unit] =
    Logger.make[IO].flatMap { implicit logger =>
      for
        config <- AppConfig.load[IO]
        _      <- Resources.make[IO](config).use { res =>
          for
            _               <- logger.info(s"starting currexx-core ${sys.env.getOrElse("VERSION", "")}")
            dispatcher      <- ActionDispatcher.make[IO].flatTap(_.dispatch(Action.RescheduleAllMonitors))
            clients         <- Clients.make[IO](config.clients, res.fs2Backend) <* logger.info("created clients")
            health          <- Health.make[IO] <* logger.info("created health")
            auth            <- Auth.make(config.auth, res.mongo, dispatcher) <* logger.info("created auth")
            signals         <- Signals.make(res.mongo, dispatcher) <* logger.info("created signals")
            monitors        <- Monitors.make(res.mongo, dispatcher) <* logger.info("created monitors")
            markets         <- Markets.make(res.mongo, dispatcher) <* logger.info("created markets")
            trades          <- Trades.make(res.mongo, clients, dispatcher) <* logger.info("created trades")
            settings        <- Settings.make(res.mongo) <* logger.info("created settings")
            http            <- Http.make[IO](health, auth, signals, monitors, markets, trades, settings)
            _               <- logger.info("creating processors")
            actionProcessor <- ActionProcessor
              .make[IO](dispatcher, monitors.service, signals.service, markets.service, trades.service, settings.service)
            logProcessor <- LogEventProcessor.make[IO](res.mongo)
            _            <- logger.info(s"starting http server on port ${config.server.port}")
            _            <- Stream(
              Server.serveEmber[IO](config.server, http.app),
              actionProcessor.run,
              logProcessor.run
            ).parJoinUnbounded.compile.drain
          yield ()
        }
      yield ()
    }
