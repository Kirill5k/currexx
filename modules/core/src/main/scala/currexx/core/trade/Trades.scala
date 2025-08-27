package currexx.core.trade

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Clients
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.trade.db.{TradeOrderRepository, TradeSettingsRepository}
import kirill5k.common.cats.Clock
import mongo4cats.database.MongoDatabase

final class Trades[F[_]] private (
    val service: TradeService[F],
    val controller: Controller[F]
)

object Trades:
  def make[F[_]: {Async, Clock}](
      database: MongoDatabase[F],
      clients: Clients[F],
      dispatcher: ActionDispatcher[F]
  ): F[Trades[F]] =
    for
      settingsRepo <- TradeSettingsRepository.make[F](database)
      orderRepo    <- TradeOrderRepository.make[F](database)
      svc          <- TradeService.make[F](settingsRepo, orderRepo, clients.broker, clients.marketData, dispatcher)
      ctrl         <- TradeController.make[F](svc)
    yield Trades[F](svc, ctrl)
