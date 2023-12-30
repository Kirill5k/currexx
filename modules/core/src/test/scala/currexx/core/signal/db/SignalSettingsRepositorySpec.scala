package currexx.core.signal.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Settings, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import mongo4cats.bson.{Document, ObjectId}
import mongo4cats.bson.syntax.*
import mongo4cats.circe.*
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SignalSettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12349

  "A SignalSettingsRepository" when {
    "get" should {
      "return error when signals do not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalSettingsRepository.make(db)
          res  <- repo.get(UserId(ObjectId().toHexString))
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Global")))
      }

      "return error when signal-settings do not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalSettingsRepository.make(db)
          res  <- repo.get(Users.uid2)
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Signal")))
      }

      "return signal-settings" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalSettingsRepository.make(db)
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Settings.signal)
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
            _    <- coll.insertOne(Document("userId" := Users.uid.toObjectId, "signal" := Settings.signal))
            res  <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
