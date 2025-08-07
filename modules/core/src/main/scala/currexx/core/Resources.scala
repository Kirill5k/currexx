package currexx.core

import cats.effect.{Async, Resource}
import cats.syntax.apply.*
import currexx.core.common.config.{AppConfig, MongoConfig}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import mongo4cats.models.client.{ConnectionString, MongoClientSettings}
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.{BackendOptions, WebSocketStreamBackend}
import sttp.client4.httpclient.fs2.HttpClientFs2Backend as Fs2Backend

import java.util.concurrent.TimeUnit
import scala.concurrent.duration.*

final class Resources[F[_]] private (
    val mongo: MongoDatabase[F],
    val fs2Backend: WebSocketStreamBackend[F, Fs2Streams[F]]
)

object Resources:

  private def mongoDb[F[_]: Async](config: MongoConfig): Resource[F, MongoDatabase[F]] =
    val settings = MongoClientSettings
      .builder()
      .applyConnectionString(ConnectionString(config.connectionUri))
      .applyToSocketSettings { builder =>
        val _ = builder
          .connectTimeout(config.connectTimeout.toMillis, TimeUnit.MILLISECONDS)
          .readTimeout(config.readTimeout.toMillis, TimeUnit.MILLISECONDS)
      }
      .applyToClusterSettings { builder =>
        val _ = builder.serverSelectionTimeout(config.serverSelectionTimeout.toMillis, TimeUnit.MILLISECONDS)
      }
      .build()
    MongoClient.create[F](settings).evalMap(_.getDatabase("currexx"))

  private def fs2Backend[F[_]: Async](timeout: FiniteDuration): Resource[F, WebSocketStreamBackend[F, Fs2Streams[F]]] =
    Fs2Backend.resource[F](options = BackendOptions(timeout, None))

  def make[F[_]: Async](config: AppConfig): Resource[F, Resources[F]] =
    (
      mongoDb[F](config.mongo),
      fs2Backend[F](3.minutes)
    ).mapN(Resources(_, _))
