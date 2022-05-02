package currexx.backtest

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.signal.{Signal, SignalService, SignalSettings}
import currexx.domain.user.UserId
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}

final private class TestSignalRepository[F[_]](using F: Monad[F]) extends SignalRepository[F]:
  override def save(signal: Signal): F[Unit]           = F.unit
  override def getAll(userId: UserId): F[List[Signal]] = F.pure(Nil)

final private class TestSignalSettingsRepository[F[_]: Async](
    private val settings: Ref[F, SignalSettings]
) extends SignalSettingsRepository[F]:
  override def update(ss: SignalSettings): F[Unit]         = settings.update(_ => ss)
  override def get(uid: UserId): F[Option[SignalSettings]] = settings.get.map(Some(_))

object TestSignalService:
  def make[F[_]: Async](initialSettings: SignalSettings, dispatcher: ActionDispatcher[F]): F[SignalService[F]] =
    Ref.of(initialSettings).flatMap(s => SignalService.make(TestSignalRepository[F], TestSignalSettingsRepository[F](s), dispatcher))
