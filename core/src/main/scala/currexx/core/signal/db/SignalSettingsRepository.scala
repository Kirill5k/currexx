package currexx.core.signal.db

import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*
import currexx.core.common.db.Repository
import currexx.domain.market.{CurrencyPair}
import currexx.domain.market.v2.Indicator
import currexx.domain.user.UserId
import currexx.core.signal.SignalSettings
import currexx.domain.errors.AppError
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.collection.MongoCollection
import mongo4cats.collection.operations.{Filter, Update}
import mongo4cats.database.MongoDatabase

import scala.jdk.CollectionConverters.*

trait SignalSettingsRepository[F[_]] extends Repository[F]:
  def update(settings: SignalSettings): F[Unit]
  def get(uid: UserId): F[SignalSettings]

final private class LiveSignalSettingsRepository[F[_]: Async](
    private val collection: MongoCollection[F, SignalSettingsEntity]
) extends SignalSettingsRepository[F] {

  override def update(settings: SignalSettings): F[Unit] =
    collection
      .updateOne(userIdEq(settings.userId), Update.set(Field.Indicators, settings.indicators.asJava))
      .map(_.getMatchedCount)
      .flatMap {
        case 0 => collection.insertOne(SignalSettingsEntity.from(settings)).void
        case _ => ().pure[F]
      }

  override def get(uid: UserId): F[SignalSettings] =
    collection
      .find(userIdEq(uid))
      .first
      .flatMap(ss => Async[F].fromOption(ss.map(_.toDomain), AppError.NotSetup("Signal")))
}

object SignalSettingsRepository extends MongoJsonCodecs:

  def make[F[_]: Async](db: MongoDatabase[F]): F[SignalSettingsRepository[F]] =
    db.getCollectionWithCodec[SignalSettingsEntity]("signal-settings")
      .map(coll => LiveSignalSettingsRepository[F](coll.withAddedCodec[CurrencyPair].withAddedCodec[Indicator]))
