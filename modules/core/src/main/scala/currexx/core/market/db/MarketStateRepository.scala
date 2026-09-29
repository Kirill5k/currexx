package currexx.core.market.db

import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import com.mongodb.MongoWriteException
import com.mongodb.client.model.ReturnDocument
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import currexx.core.common.db.Repository
import currexx.core.market.{MarketProfile, MarketState, PositionState}
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.models.collection.{FindOneAndUpdateOptions, IndexOptions, UpdateOptions}
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.database.MongoDatabase

trait MarketStateRepository[F[_]]:
  // Returns false when the stored state no longer matches the snapshot. A state without a version is created.
  def save(state: MarketState): F[Boolean]
  def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState]
  def getAll(uid: UserId): F[List[MarketState]]
  def deleteAll(uid: UserId): F[Unit]
  def delete(uid: UserId, cp: CurrencyPair): F[Unit]
  def find(uid: UserId, cp: CurrencyPair): F[Option[MarketState]]

final private class LiveMarketStateRepository[F[_]](
    private val collection: MongoCollection[F, MarketStateEntity]
)(using
    F: Async[F]
) extends MarketStateRepository[F] with Repository[F] {
  private val VersionField  = "version"
  private val updateOptions = FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER, upsert = true)

  override def deleteAll(uid: UserId): F[Unit] =
    collection.deleteMany(userIdEq(uid)).void

  override def delete(uid: UserId, cp: CurrencyPair): F[Unit] =
    collection
      .deleteOne(userIdAndCurrencyPairEq(uid, cp))
      .flatMap(errorIfNotDeleted(AppError.NotTracked(List(cp))))

  // Version 0 is a document stored before versioning was introduced. createdAt tells apart a state that was deleted
  // and recreated, whose version starts again from 1.
  private def sameSnapshot(state: MarketState): Filter =
    state.version match
      case None          => Filter.notExists(VersionField)
      case Some(0L)      => Filter.notExists(VersionField) && Filter.eq("createdAt", state.createdAt)
      case Some(version) => Filter.eq(VersionField, version) && Filter.eq("createdAt", state.createdAt)

  override def save(state: MarketState): F[Boolean] =
    collection
      .updateOne(
        userIdAndCurrencyPairEq(state.userId, state.currencyPair) && sameSnapshot(state),
        Update
          .set("profile", state.profile)
          .set("previousProfile", state.previousProfile)
          .set("currentPosition", state.currentPosition)
          .set("lastCandleTime", state.lastCandleTime)
          .set("lastTimeStateCandle", state.lastTimeStateCandle)
          .inc(VersionField, 1)
          .currentDate(Repository.Field.LastUpdatedAt)
          .setOnInsert("userId", state.userId.toObjectId)
          .setOnInsert("currencyPair", state.currencyPair)
          .setOnInsert("createdAt", state.createdAt),
        UpdateOptions(upsert = state.version.isEmpty)
      )
      .map(res => res.getMatchedCount > 0 || res.getUpsertedId != null)
      // A rejected upsert of a new state collides with the unique user/pair index: the state was created concurrently.
      .recover { case error: MongoWriteException if error.getError.getCode == 11000 => false }

  override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState] =
    collection
      .findOneAndUpdate(
        userIdAndCurrencyPairEq(uid, pair),
        Update
          .set("currentPosition", position)
          .inc(VersionField, 1)
          .currentDate(Repository.Field.LastUpdatedAt)
          .setOnInsert("userId", uid.toObjectId)
          .setOnInsert("currencyPair", pair)
          .setOnInsert("createdAt", java.time.Instant.now())
          .setOnInsert("profile", MarketProfile()),
        updateOptions
      )
      .flatMap(opt => F.fromOption(opt, AppError.Internal("could not upsert market state")))
      .map(_.toDomain)

  override def getAll(uid: UserId): F[List[MarketState]] =
    collection
      .find(userIdEq(uid))
      .all
      .mapList(_.toDomain)

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    collection
      .find(userIdAndCurrencyPairEq(uid, pair))
      .first
      .mapOpt(_.toDomain)
}

object MarketStateRepository extends MongoJsonCodecs {
  val indexByUidAndCp = Index.ascending(Repository.Field.UId).combinedWith(Index.ascending(Repository.Field.CurrencyPair))
  val indexByUid      = Index.ascending(Repository.Field.UId)

  def make[F[_]: Async](db: MongoDatabase[F]): F[MarketStateRepository[F]] =
    db.getCollectionWithCodec[MarketStateEntity](Repository.Collection.MarketState)
      .flatTap(_.createIndex(indexByUidAndCp, IndexOptions().unique(true)))
      .flatTap(_.createIndex(indexByUid))
      .map(_.withAddedCodec[CurrencyPair].withAddedCodec[MarketProfile].withAddedCodec[PositionState])
      .map(coll => LiveMarketStateRepository[F](coll))
}
