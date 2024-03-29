package currexx.core.monitor

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.monitor.db.MonitorRepository
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase

final class Monitors[F[_]] private (
    val service: MonitorService[F],
    val controller: Controller[F]
)

object Monitors:
  def make[F[_]: Async: Clock](
      database: MongoDatabase[F],
      dispatcher: ActionDispatcher[F]
  ): F[Monitors[F]] =
    for
      repo <- MonitorRepository.make[F](database)
      svc  <- MonitorService.make[F](repo, dispatcher)
      ctrl <- MonitorController.make[F](svc)
    yield Monitors[F](svc, ctrl)
