package currexx.core.settings

import cats.MonadThrow
import cats.syntax.flatMap.*
import currexx.core.settings.db.SettingsRepository
import currexx.domain.user.UserId

/** Loads a validated, internally consistent snapshot of the signal and trade settings. */
trait StrategyRuntime[F[_]]:
  def load(uid: UserId): F[StrategyConfiguration]

object StrategyRuntime:
  def make[F[_]](settings: SettingsRepository[F])(using F: MonadThrow[F]): StrategyRuntime[F] =
    new StrategyRuntime[F]:
      def load(uid: UserId): F[StrategyConfiguration] =
        settings.get(uid).flatMap(s => F.fromEither(StrategyConfiguration.from(s)))
