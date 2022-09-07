package currexx.core.market.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import io.circe.syntax.*
import com.mongodb.client.model.ReturnDocument
import currexx.domain.market.{CurrencyPair, PriceRange}
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import currexx.core.common.db.Repository
import currexx.core.market.{IndicatorState, MarketState, PositionState}
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.models.collection.FindOneAndUpdateOptions
import mongo4cats.collection.MongoCollection
import mongo4cats.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

trait MarketStateRepository[F[_]] extends Repository[F]:
  def update(uid: UserId, pair: CurrencyPair, signals: Map[String, List[IndicatorState]]): F[MarketState]
  def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[MarketState]
  def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState]
  def getAll(uid: UserId): F[List[MarketState]]
  def deleteAll(uid: UserId): F[Unit]
  def delete(uid: UserId, cp: CurrencyPair): F[Unit]
  def find(uid: UserId, cp: CurrencyPair): F[Option[MarketState]]

final private class LiveMarketStateRepository[F[_]](
    private val collection: MongoCollection[F, MarketStateEntity]
)(using
    F: Async[F]
) extends MarketStateRepository[F] {

  private val updateOptions = FindOneAndUpdateOptions(returnDocument = ReturnDocument.AFTER)

  override def deleteAll(uid: UserId): F[Unit] =
    collection.deleteMany(userIdEq(uid)).void

  override def delete(uid: UserId, cp: CurrencyPair): F[Unit] =
    collection
      .deleteOne(userIdAndCurrencyPairEq(uid, cp))
      .flatMap(errorIfNotDeleted(AppError.NotTracked(cp)))

  override def update(uid: UserId, cp: CurrencyPair, signals: Map[String, List[IndicatorState]]): F[MarketState] =
    runUpdate(
      userIdAndCurrencyPairEq(uid, cp),
      Update.set("signals", signals),
      MarketStateEntity.make(uid, cp, signals = signals)
    )

  override def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[MarketState] =
    runUpdate(
      userIdAndCurrencyPairEq(uid, pair),
      Update.set("latestPrice", price),
      MarketStateEntity.make(uid, pair, latestPrice = Some(price))
    )

  override def update(uid: UserId, pair: CurrencyPair, position: Option[PositionState]): F[MarketState] =
    runUpdate(
      userIdAndCurrencyPairEq(uid, pair),
      Update.set("currentPosition", position),
      MarketStateEntity.make(uid, pair, currentPosition = position)
    )

  private def runUpdate(
      filter: Filter,
      update: Update,
      default: => MarketStateEntity
  ): F[MarketState] =
    collection
      .findOneAndUpdate(filter, update.currentDate(Field.LastUpdatedAt), updateOptions)
      .flatMap {
        case Some(state) => state.toDomain.pure[F]
        case None        => collection.insertOne(default).as(default.toDomain)
      }

  override def getAll(uid: UserId): F[List[MarketState]] =
    collection.find(userIdEq(uid)).all.mapIterable(_.toDomain)

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    collection.find(userIdAndCurrencyPairEq(uid, pair)).first.mapOption(_.toDomain)
}

object MarketStateRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MarketStateRepository[F]] =
    db.getCollectionWithCodec[MarketStateEntity]("market-state")
      .map(_.withAddedCodec[CurrencyPair].withAddedCodec[IndicatorState].withAddedCodec[PriceRange].withAddedCodec[PositionState])
      .map(coll => LiveMarketStateRepository[F](coll))
