package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.core.common.db.Repository.Field
import currexx.core.common.http.SearchParams
import currexx.core.signal.Signal
import currexx.domain.market.CurrencyPair
import currexx.domain.signal.Indicator
import currexx.domain.user.UserId
import kirill5k.common.cats.syntax.applicative.*
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.database.CreateCollectionOptions
import mongo4cats.operations.Index

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
      .find(searchBy(uid, sp, Field.CurrencyPair))
      .sortByDesc(Field.Time)
      .all
      .mapList(_.toDomain)
}

object SignalRepository extends MongoJsonCodecs:
  private val collectionName    = Repository.Collection.Signals
  private val collectionOptions = CreateCollectionOptions(capped = true, sizeInBytes = 268435456L)

  val indexByUid = Index.ascending(Field.UId)
  val indexByCp  = indexByUid.combinedWith(Index.ascending(Field.CurrencyPair)).combinedWith(Index.ascending(Field.Time))

  def make[F[_]](db: MongoDatabase[F])(using F: Async[F]): F[SignalRepository[F]] =
    for
      collNames <- db.listCollectionNames
      _         <- F.unlessA(collNames.toSet.contains(collectionName))(db.createCollection(collectionName, collectionOptions))
      coll      <- db.getCollectionWithCodec[SignalEntity](collectionName)
      _         <- coll.createIndex(indexByUid)
      _         <- coll.createIndex(indexByCp)
    yield LiveSignalRepository[F](coll.withAddedCodec[CurrencyPair].withAddedCodec[Indicator])
