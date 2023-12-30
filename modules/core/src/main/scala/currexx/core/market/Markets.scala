package currexx.core.market

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.http.Controller
import currexx.core.market.db.MarketStateRepository
import mongo4cats.database.MongoDatabase

final class Markets[F[_]] private (
    val service: MarketService[F],
    val controller: Controller[F]
)

object Markets:
  def make[F[_]: Async](
      database: MongoDatabase[F],
      dispatcher: ActionDispatcher[F]
  ): F[Markets[F]] =
    for
      stateRepo <- MarketStateRepository.make[F](database)
      svc       <- MarketService.make[F](stateRepo, dispatcher)
      ctrl      <- MarketController.make[F](svc)
    yield Markets[F](svc, ctrl)
