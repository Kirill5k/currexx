package currexx.backtest.services

import cats.Monad
import cats.effect.{Async, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.SearchParams
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.core.signal.{Signal, SignalService, SignalSettings}
import currexx.domain.time.Clock
import currexx.domain.user.UserId

final private class TestSignalRepository[F[_]](using F: Monad[F]) extends SignalRepository[F]:
  override def saveAll(signals: List[Signal]): F[Unit]                   = F.unit
  override def isFirstOfItsKindForThatDate(signal: Signal): F[Boolean]   = F.pure(true)
  override def getAll(userId: UserId, sp: SearchParams): F[List[Signal]] = F.pure(Nil)

final private class TestSignalSettingsRepository[F[_]: Async](
    private val settings: Ref[F, SignalSettings]
) extends SignalSettingsRepository[F]:
  override def update(ss: SignalSettings): F[Unit] = settings.update(_ => ss)
  override def get(uid: UserId): F[SignalSettings] = settings.get

object TestSignalService:
  def make[F[_]: Async](initialSettings: SignalSettings, dispatcher: ActionDispatcher[F]): F[SignalService[F]] =
    Ref.of(initialSettings).flatMap(s => SignalService.make(TestSignalRepository[F], TestSignalSettingsRepository[F](s), dispatcher))
