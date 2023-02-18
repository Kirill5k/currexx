package currexx.core.trade.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.clients.broker.BrokerParameters
import currexx.core.MongoSpec
import currexx.core.fixtures.{Settings, Trades, Users}
import currexx.core.trade.db.TradeSettingsRepository
import currexx.core.trade.TradeStrategy
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.bson.{Document, ObjectId}
import mongo4cats.bson.syntax.*
import mongo4cats.circe.*
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class TradeSettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12350

  "A TradeSettingsRepository" when {
    "get" should {
      "return error when global-settings do not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          res  <- repo.get(UserId(ObjectId().toHexString))
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Global")))
      }

      "return error when trade-settings do not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          res  <- repo.get(Users.uid2)
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Trade")))
      }

      "return trade-settings" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Settings.trade)
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use { client =>
          for
            db   <- client.getDatabase("currexx")
            coll <- db.getCollection("settings")
            _    <- coll.insertOne(Document("userId" := Users.uid2.toObjectId))
            _    <- coll.insertOne(Document("userId" := Users.uid.toObjectId, "trade" := Settings.trade))
            res  <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
