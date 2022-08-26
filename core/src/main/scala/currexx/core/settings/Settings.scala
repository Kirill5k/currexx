package currexx.core.settings

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.http.Controller
import currexx.core.settings.db.SettingsRepository
import mongo4cats.database.MongoDatabase

final class Settings[F[_]] private (
    val service: SettingsService[F],
    val controller: Controller[F]
)

object Settings:
  def make[F[_]: Async](
      database: MongoDatabase[F]
  ): F[Settings[F]] =
    for
      settingsRepo <- SettingsRepository.make[F](database)
      svc          <- SettingsService.make[F](settingsRepo)
      ctrl         <- SettingsController.make[F](svc)
    yield Settings[F](svc, ctrl)
