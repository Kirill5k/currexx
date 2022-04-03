package currexx.core

import cats.effect.{Async, Resource}
import cats.syntax.apply.*
import currexx.core.common.config.{AppConfig, MongoConfig}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase
import sttp.client3.asynchttpclient.cats.AsyncHttpClientCatsBackend
import sttp.client3.{SttpBackend, SttpBackendOptions}

import scala.concurrent.duration.*

final class Resources[F[_]] private (
    val mongo: MongoDatabase[F],
    val sttpBackend: SttpBackend[F, Any]
)

object Resources:
  private def mongoDb[F[_]: Async](config: MongoConfig): Resource[F, MongoDatabase[F]] =
    MongoClient
      .fromConnectionString[F](config.connectionUri)
      .evalMap(_.getDatabase("currexx"))

  private def sttpBackend[F[_]: Async]: Resource[F, SttpBackend[F, Any]] =
    Resource.make(AsyncHttpClientCatsBackend[F](SttpBackendOptions(connectionTimeout = 3.minutes, proxy = None)))(_.close())

  def make[F[_]: Async](config: AppConfig): Resource[F, Resources[F]] =
    (
      mongoDb[F](config.mongo),
      sttpBackend[F]
    ).mapN((mongo, sttp) => Resources(mongo, sttp))
