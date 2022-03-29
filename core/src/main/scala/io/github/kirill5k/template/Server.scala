package io.github.kirill5k.template

import cats.effect.Async
import fs2.Stream
import org.http4s.{HttpApp, HttpRoutes}
import org.http4s.blaze.server.BlazeServerBuilder
import io.github.kirill5k.template.common.config.ServerConfig

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.*

object Server:
  def serve[F[_]: Async](config: ServerConfig, routes: HttpApp[F], ec: ExecutionContext): Stream[F, Unit] =
    BlazeServerBuilder[F]
      .withExecutionContext(ec)
      .bindHttp(config.port, config.host)
      .withResponseHeaderTimeout(3.minutes)
      .withIdleTimeout(1.hour)
      .withHttpApp(routes)
      .serve
      .drain
