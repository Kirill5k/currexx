package currexx.core.signal

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.Resources
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.domain.time.Clock
import mongo4cats.database.MongoDatabase

final class Signals[F[_]] private (
    val service: SignalService[F],
    val controller: Controller[F]
)

object Signals:
  def make[F[_]: Async: Clock](database: MongoDatabase[F], dispatcher: ActionDispatcher[F]): F[Signals[F]] =
    for
      signRepo <- SignalRepository.make[F](database)
      settRepo <- SignalSettingsRepository.make[F](database)
      svc      <- SignalService.make[F](signRepo, settRepo, dispatcher)
      ctrl     <- SignalController.make[F](svc)
    yield Signals[F](svc, ctrl)
