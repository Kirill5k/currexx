package currexx.core.market.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import com.mongodb.client.model.ReturnDocument
import currexx.domain.market.{CurrencyPair, Indicator, PriceRange}
import currexx.domain.user.UserId
import currexx.domain.errors.AppError
import currexx.core.common.db.Repository
import currexx.core.market.{IndicatorState, MarketState}
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.{FindOneAndUpdateOptions, MongoCollection}
import mongo4cats.collection.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

import scala.jdk.CollectionConverters.*

trait MarketStateRepository[F[_]] extends Repository[F]:
  def update(uid: UserId, pair: CurrencyPair, signals: Map[Indicator, List[IndicatorState]]): F[MarketState]
  def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[MarketState]
  def getAll(uid: UserId): F[List[MarketState]]
  def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]]

final private class LiveMarketStateRepository[F[_]](
    private val collection: MongoCollection[F, MarketStateEntity]
)(using
    F: Async[F]
) extends MarketStateRepository[F] {

  private val updateOptions = FindOneAndUpdateOptions().returnDocument(ReturnDocument.AFTER)

  override def update(uid: UserId, pair: CurrencyPair, signals: Map[Indicator, List[IndicatorState]]): F[MarketState] =
    runUpdate(
      userIdAndCurrencyPairEq(uid, pair),
      Update.set("signals", signals.map((k, v) => k.toString -> v).asJava),
      MarketStateEntity.make(uid, pair, signals = signals)
    )

  override def update(uid: UserId, pair: CurrencyPair, price: PriceRange): F[MarketState] =
    runUpdate(
      userIdAndCurrencyPairEq(uid, pair),
      Update.set("latestPrice", price),
      MarketStateEntity.make(uid, pair, latestPrice = Some(price))
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
        case None => collection.insertOne(default).as(default.toDomain)
      }

  override def getAll(uid: UserId): F[List[MarketState]] =
    collection.find(userIdEq(uid)).all.map(_.map(_.toDomain).toList)

  override def find(uid: UserId, pair: CurrencyPair): F[Option[MarketState]] =
    collection.find(userIdAndCurrencyPairEq(uid, pair)).first.map(_.map(_.toDomain))
}

object MarketStateRepository extends MongoJsonCodecs:
  def make[F[_]: Async](db: MongoDatabase[F]): F[MarketStateRepository[F]] =
    db.getCollectionWithCodec[MarketStateEntity]("market-state")
      .map(coll => LiveMarketStateRepository[F](coll.withAddedCodec[CurrencyPair].withAddedCodec[PriceRange]))
