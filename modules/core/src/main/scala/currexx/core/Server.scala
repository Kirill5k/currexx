package currexx.core

import cats.effect.Async
import com.comcast.ip4s.*
import fs2.Stream
import fs2.io.net.Network
import org.http4s.HttpApp
import org.http4s.ember.server.EmberServerBuilder
import currexx.core.common.config.ServerConfig

import scala.concurrent.duration.*

object Server:
  def serve[F[_]](config: ServerConfig, routes: HttpApp[F])(using F: Async[F]): Stream[F, Unit] =
    Stream.eval {
      EmberServerBuilder
        .default(F, Network.forAsync[F])
        .withHostOption(Ipv4Address.fromString(config.host))
        .withPort(Port.fromInt(config.port).get)
        .withIdleTimeout(1.hour)
        .withHttpApp(routes)
        .build
        .use(_ => Async[F].never)
    }
