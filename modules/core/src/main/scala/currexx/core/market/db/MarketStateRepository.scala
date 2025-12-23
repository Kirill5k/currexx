package currexx.core.market.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import com.mongodb.client.model.ReturnDocument
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import currexx.core.common.db.Repository
import currexx.core.market.{MarketProfile, MarketState, PositionState}
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.models.collection.{FindOneAndUpdateOptions, IndexOptions}
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Index, Update}
import mongo4cats.database.MongoDatabase

trait MarketStateRepository[F[_]]:
  def update(uid: UserId, pair: CurrencyPair, profile: MarketProfile): F[MarketState]
  def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState]
  def update(uid: UserId, pair: CurrencyPair, profile: MarketProfile, position: Option[PositionState]): F[MarketState]
  def getAll(uid: UserId): F[List[MarketState]]
  def deleteAll(uid: UserId): F[Unit]
  def delete(uid: UserId, cp: CurrencyPair): F[Unit]
  def find(uid: UserId, cp: CurrencyPair): F[Option[MarketState]]

final private class LiveMarketStateRepository[F[_]](
    private val collection: MongoCollection[F, MarketStateEntity]
)(using
    F: Async[F]
) extends MarketStateRepository[F] with Repository[F] {

  private val updateOptions = FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER, upsert = true)

  override def deleteAll(uid: UserId): F[Unit] =
    collection.deleteMany(userIdEq(uid)).void

  override def delete(uid: UserId, cp: CurrencyPair): F[Unit] =
    collection
      .deleteOne(userIdAndCurrencyPairEq(uid, cp))
      .flatMap(errorIfNotDeleted(AppError.NotTracked(List(cp))))

  override def update(uid: UserId, cp: CurrencyPair, profile: MarketProfile): F[MarketState] =
    collection
      .findOneAndUpdate(
        userIdAndCurrencyPairEq(uid, cp),
        Update
          .set("profile", profile)
          .currentDate(Repository.Field.LastUpdatedAt)
          .setOnInsert("userId", uid.toObjectId)
          .setOnInsert("currencyPair", cp)
          .setOnInsert("createdAt", java.time.Instant.now()),
        updateOptions
      )
      .flatMap(opt => F.fromOption(opt, AppError.Internal("could not upsert market state")))
      .map(_.toDomain)

  override def update(uid: UserId, pair: CurrencyPair, profile: MarketProfile, position: Option[PositionState]): F[MarketState] =
    collection
      .findOneAndUpdate(
        userIdAndCurrencyPairEq(uid, pair),
        Update
          .set("profile", profile)
          .set("currentPosition", position)
          .currentDate(Repository.Field.LastUpdatedAt)
          .setOnInsert("userId", uid.toObjectId)
          .setOnInsert("currencyPair", pair)
          .setOnInsert("createdAt", java.time.Instant.now()),
        updateOptions
      )
      .flatMap(opt => F.fromOption(opt, AppError.Internal("could not upsert market state")))
      .map(_.toDomain)

  override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState] =
    collection
      .findOneAndUpdate(
        userIdAndCurrencyPairEq(uid, pair),
        Update
          .set("currentPosition", position)
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
      .mapIterable(_.toDomain)

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    collection
      .find(userIdAndCurrencyPairEq(uid, pair))
      .first
      .mapOption(_.toDomain)
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
