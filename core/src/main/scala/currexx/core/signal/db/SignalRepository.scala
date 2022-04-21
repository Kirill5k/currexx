package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.signal.Signal
import currexx.domain.market.CurrencyPair
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.Filter
import mongo4cats.database.{CreateCollectionOptions, MongoDatabase}

trait SignalRepository[F[_]] extends Repository[F]:
  def save(signal: Signal): F[Unit]
  def getAll(userId: UserId): F[List[Signal]]

final private class LiveSignalRepository[F[_]: Async](
    private val collection: MongoCollection[F, SignalEntity]
) extends SignalRepository[F] {

  override def save(signal: Signal): F[Unit] =
    collection.insertOne(SignalEntity.from(signal)).void

  override def getAll(userId: UserId): F[List[Signal]] =
    collection.find(userIdEq(userId)).all.map(_.map(_.toDomain).toList)
}

object SignalRepository extends MongoJsonCodecs:
  private val collectionName    = "signals"
  private val collectionOptions = CreateCollectionOptions().capped(true).sizeInBytes(268435456L)

  def make[F[_]: Async](db: MongoDatabase[F]): F[SignalRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- if (collNames.toSet.contains(collectionName)) ().pure[F] else db.createCollection(collectionName, collectionOptions)
      coll      <- db.getCollectionWithCodec[SignalEntity](collectionName)
    yield LiveSignalRepository[F](coll.withAddedCodec[CurrencyPair])
