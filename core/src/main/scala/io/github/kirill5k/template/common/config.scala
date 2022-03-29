package io.github.kirill5k.template.common

import cats.effect.Sync
import pureconfig.*
import pureconfig.generic.derivation.default.*

object config {

  final case class ServerConfig(
      host: String,
      port: Int
  ) derives ConfigReader

  final case class AppConfig(
      server: ServerConfig
  ) derives ConfigReader

  object AppConfig {
    def load[F[_]](implicit F: Sync[F]): F[AppConfig] =
      F.blocking(ConfigSource.default.loadOrThrow[AppConfig])
  }
}
