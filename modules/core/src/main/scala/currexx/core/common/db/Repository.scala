package currexx.core.common.db

import cats.{Functor, MonadError}
import cats.syntax.functor.*
import com.mongodb.client.result.{DeleteResult, UpdateResult}
import currexx.core.common.http.SearchParams
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId
import mongo4cats.operations.Filter

trait Repository[F[_]] {

  protected object Field {
    val Id                = "_id"
    val Name              = "name"
    val Time              = "time"
    val UId               = "userId"
    val Email             = "email"
    val Kind              = "kind"
    val LastQueriedAt     = "lastQueriedAt"
    val LastUpdatedAt     = "lastUpdatedAt"
    val Status            = "status"
    val LastAccessedAt    = "lastAccessedAt"
    val Active            = "active"
    val CurrencyPair      = "currencyPair"
    val CurrencyPairs     = "currencyPairs"
    val OrderCurrencyPair = "order.currencyPair"
    val Indicators        = "indicators"
    val TriggeredBy       = "triggeredBy"
  }

  private def idEqFilter(name: String, id: String): Filter = Filter.eq(name, ObjectId(id))
  protected def idEq(id: String): Filter                   = idEqFilter(Field.Id, id)
  protected def userIdEq(uid: UserId): Filter              = idEqFilter(Field.UId, uid.value)

  protected def userIdAndCurrencyPairEq(uid: UserId, pair: CurrencyPair): Filter =
    userIdEq(uid) && Filter.eq(Field.CurrencyPair, pair)

  protected def searchBy(uid: UserId, sp: SearchParams): Filter =
    List(
      sp.from.map(f => Filter.gte(Field.Time, f)),
      sp.to.map(t => Filter.lt(Field.Time, t)),
      sp.currencyPair.map(cp => Filter.regex(Field.CurrencyPair, s"${cp.base.code}\\/?${cp.quote.code}"))
    ).flatten.foldLeft(userIdEq(uid))(_ && _)

  protected def errorIfNotDeleted(error: Throwable)(res: DeleteResult)(using F: MonadError[F, Throwable]): F[Unit] =
    F.raiseWhen(res.getDeletedCount == 0)(error)

  protected def errorIfNoMatches(error: Throwable)(res: UpdateResult)(using F: MonadError[F, Throwable]): F[Unit] =
    F.raiseWhen(res.getMatchedCount == 0)(error)

  extension [A](foa: F[Option[A]]) def mapOption[B](fab: A => B)(using F: Functor[F]): F[Option[B]]   = foa.map(_.map(fab))
  extension [A](fia: F[Iterable[A]]) def mapIterable[B](fab: A => B)(using F: Functor[F]): F[List[B]] = fia.map(_.toList.map(fab))
}

object Repository {
  object Collection {
    val Sessions      = "sessions"
    val Users         = "users"
    val LogEvents     = "log-events"
    val MarketState   = "market-state"
    val Monitors      = "monitors"
    val Settings      = "settings"
    val Signals       = "signals"
    val TradeOrders   = "trade-orders"
  }
}
