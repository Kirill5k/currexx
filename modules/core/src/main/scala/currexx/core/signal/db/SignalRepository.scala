package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.http.SearchParams
import currexx.core.signal.Signal
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import currexx.domain.user.UserId
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.models.database.CreateCollectionOptions
import mongo4cats.database.MongoDatabase

trait SignalRepository[F[_]] extends Repository[F]:
  def saveAll(signals: List[Signal]): F[Unit]
  def getAll(userId: UserId, sp: SearchParams): F[List[Signal]]

final private class LiveSignalRepository[F[_]: Async](
    private val collection: MongoCollection[F, SignalEntity]
) extends SignalRepository[F] {

  override def saveAll(signals: List[Signal]): F[Unit] =
    collection.insertMany(signals.map(SignalEntity.from)).void

  override def getAll(uid: UserId, sp: SearchParams): F[List[Signal]] =
    collection
      .find(searchBy(uid, sp))
      .sortByDesc(Field.Time)
      .all
      .mapIterable(_.toDomain)
}

object SignalRepository extends MongoJsonCodecs:
  private val collectionName    = "signals"
  private val collectionOptions = CreateCollectionOptions(capped = true, sizeInBytes = 268435456L)

  def make[F[_]](db: MongoDatabase[F])(using F: Async[F]): F[SignalRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- F.unlessA(collNames.toSet.contains(collectionName))(db.createCollection(collectionName, collectionOptions))
      coll      <- db.getCollectionWithCodec[SignalEntity](collectionName)
    yield LiveSignalRepository[F](coll.withAddedCodec[CurrencyPair].withAddedCodec[Indicator])
