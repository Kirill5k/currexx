package currexx.core

import cats.effect.{Async, Resource}
import cats.syntax.apply.*
import currexx.core.common.config.{AppConfig, MongoConfig}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.models.client.{ConnectionString, MongoClientSettings}
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.{SttpBackend, SttpBackendOptions}

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

final class Resources[F[_]] private (
    val mongo: MongoDatabase[F],
    val sttpBackend: SttpBackend[F, Fs2Streams[F] with WebSockets]
)

object Resources:

  private def mongoDb[F[_]: Async](config: MongoConfig): Resource[F, MongoDatabase[F]] =
    val settings = MongoClientSettings
      .builder()
      .applyConnectionString(ConnectionString(config.connectionUri))
      .applyToSocketSettings { builder =>
        val _ = builder.connectTimeout(3, TimeUnit.MINUTES).readTimeout(3, TimeUnit.MINUTES)
      }
      .applyToClusterSettings { builder =>
        val _ = builder.serverSelectionTimeout(3, TimeUnit.MINUTES)
      }
      .build()
    MongoClient.create[F](settings).evalMap(_.getDatabase("currexx"))

  private def sttpBackend[F[_]: Async]: Resource[F, SttpBackend[F, Fs2Streams[F] with WebSockets]] =
    HttpClientFs2Backend.resource[F](SttpBackendOptions(connectionTimeout = 3.minutes, proxy = None))

  def make[F[_]: Async](config: AppConfig): Resource[F, Resources[F]] =
    (
      mongoDb[F](config.mongo),
      sttpBackend[F]
    ).mapN(Resources(_, _))
