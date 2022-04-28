package currexx.core.market

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.clients.Clients
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.market.db.{MarketSettingsRepository, MarketStateRepository}
import mongo4cats.database.MongoDatabase

final class Markets[F[_]] private (
    val service: MarketService[F],
    val controller: Controller[F]
)

object Markets:
  def make[F[_]: Async](
      database: MongoDatabase[F],
      clients: Clients[F],
      dispatcher: ActionDispatcher[F]
  ): F[Markets[F]] =
    for
      settingsRepo <- MarketSettingsRepository.make[F](database)
      stateRepo    <- MarketStateRepository.make[F](database)
      svc          <- MarketService.make[F](settingsRepo, stateRepo, dispatcher)
      ctrl         <- MarketController.make[F](svc)
    yield Markets[F](svc, ctrl)
