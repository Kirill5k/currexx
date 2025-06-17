package currexx.core.common

import cats.effect.Sync
import currexx.clients.ClientsConfig
import pureconfig.{ConfigReader, ConfigSource}

object config {
  final case class JwtConfig(
      alg: String,
      secret: String
  ) derives ConfigReader

  final case class AuthConfig(
      passwordSalt: String,
      jwt: JwtConfig
  ) derives ConfigReader

  final case class MongoConfig(
      connectionUri: String
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
