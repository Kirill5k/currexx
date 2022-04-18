package currexx.core.monitor

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Clients
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.monitor.db.MonitorRepository
import mongo4cats.database.MongoDatabase

final class Monitors[F[_]] private (
    val service: MonitorService[F],
    val controller: Controller[F]
)

object Monitors:
  def make[F[_]: Async](
      database: MongoDatabase[F],
      clients: Clients[F],
      dispatcher: ActionDispatcher[F]
  ): F[Monitors[F]] =
    for
      repo <- MonitorRepository.make[F](database)
      svc  <- MonitorService.make[F](repo, dispatcher, clients.marketData)
      ctrl <- MonitorController.make[F](svc)
    yield Monitors[F](svc, ctrl)
