package currexx.core.trade

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Clients
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.trade.db.TradeSettingsRepository
import mongo4cats.database.MongoDatabase

final class Trades[F[_]] private (
    val service: TradeService[F],
    val controller: Controller[F]
)

object Trades:
  def make[F[_]: Async](
      database: MongoDatabase[F],
      clients: Clients[F],
      dispatcher: ActionDispatcher[F]
  ): F[Trades[F]] =
    for
      settingsRepo <- TradeSettingsRepository.make[F](database)
      svc          <- TradeService.make[F](settingsRepo, clients.broker, dispatcher)
      ctrl         <- TradeController.make[F](svc)
    yield Trades[F](svc, ctrl)
