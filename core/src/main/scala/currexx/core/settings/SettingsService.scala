package currexx.core.settings

import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import currexx.core.settings.db.SettingsRepository
import currexx.domain.errors.AppError
import currexx.domain.user.UserId

trait SettingsService[F[_]] {
  def get(uid: UserId): F[GlobalSettings]
  def update(gs: GlobalSettings): F[Unit]
  def createFor(uid: UserId): F[Unit]
}

final private class LiveSettingsService[F[_]](
    private val repository: SettingsRepository[F]
)(using
    F: Async[F]
) extends SettingsService[F] {
  override def get(uid: UserId): F[GlobalSettings] =
    repository.get(uid).recoverWith { case _: AppError.NotSetup => repository.createFor(uid) >> get(uid) }

  override def update(gs: GlobalSettings): F[Unit] =
    repository.update(gs)

  override def createFor(uid: UserId): F[Unit] =
    repository.createFor(uid)
}

object SettingsService:
  def make[F[_]: Async](repository: SettingsRepository[F]): F[SettingsService[F]] =
    Async[F].pure(LiveSettingsService(repository))
