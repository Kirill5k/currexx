package currexx.core.common

import cats.effect.Sync
import currexx.clients.ClientsConfig
import pureconfig.{ConfigReader, ConfigSource}

import scala.concurrent.duration.FiniteDuration

object config {

  /** Wraps a sensitive value so it is never leaked via [[toString]] (e.g. in log output). */
  final case class Secret[A](value: A):
    override def toString: String = "<SECRET>"

  object Secret:
    given [A: ConfigReader]: ConfigReader[Secret[A]] = ConfigReader[A].map(Secret(_))

  final case class JwtConfig(
      alg: String,
      secret: Secret[String]
  ) derives ConfigReader

  final case class AuthConfig(
      passwordSalt: Secret[String],
      jwt: JwtConfig
  ) derives ConfigReader

  final case class MongoConfig(
      connectionUri: String,
      connectTimeout: FiniteDuration,
      readTimeout: FiniteDuration,
      serverSelectionTimeout: FiniteDuration
  ) derives ConfigReader

  final case class ServerConfig(
      host: String,
      port: Int
  ) derives ConfigReader

  final case class AppConfig(
      server: ServerConfig,
      auth: AuthConfig,
      mongo: MongoConfig,
      clients: ClientsConfig
  ) derives ConfigReader

  object AppConfig {
    def load[F[_]: Sync]: F[AppConfig] =
      Sync[F].blocking(ConfigSource.default.loadOrThrow[AppConfig])
  }
}
