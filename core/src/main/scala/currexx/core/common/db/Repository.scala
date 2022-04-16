package currexx.core.common.db

import cats.MonadError
import cats.syntax.option.*
import com.mongodb.client.result.{DeleteResult, UpdateResult}
import currexx.domain.types.IdType
import currexx.domain.user.UserId
import mongo4cats.bson.ObjectId
import mongo4cats.collection.operations.{Filter, Update}

import java.time.Instant

trait Repository[F[_]] {

  protected object Field {
    val Id             = "_id"
    val Name           = "name"
    val UId            = "userId"
    val Email          = "email"
    val LastUpdatedAt  = "lastUpdatedAt"
    val Status         = "status"
    val LastAccessedAt = "lastAccessedAt"
    val Active         = "active"
    val CurrencyPair   = "currencyPair"
  }

  private def idEqFilter(name: String, id: Option[String]): Filter = Filter.eq(name, id.map(ObjectId.apply).orNull)
  protected def idEq(id: String): Filter                           = idEqFilter(Field.Id, id.some)
  protected def userIdEq(uid: Option[UserId]): Filter              = idEqFilter(Field.UId, uid.map(_.value))
  protected def userIdEq(uid: UserId): Filter                      = idEqFilter(Field.UId, uid.value.some)

  protected def errorIfNull[A](error: Throwable)(res: A)(using F: MonadError[F, Throwable]): F[A] =
    F.fromOption(Option(res), error)

  protected def errorIfNotDeleted(error: Throwable)(res: DeleteResult)(using F: MonadError[F, Throwable]): F[Unit] =
    F.raiseWhen(res.getDeletedCount == 0)(error)

  protected def errorIfNoMatches(error: Throwable)(res: UpdateResult)(using F: MonadError[F, Throwable]): F[Unit] =
    F.raiseWhen(res.getMatchedCount == 0)(error)
}
