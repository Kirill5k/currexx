package currexx.core.settings.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.syntax.given
import currexx.core.fixtures.{Settings, Users}
import currexx.core.MongoSpec
import currexx.core.settings.{GlobalSettings, SignalSettings}
import currexx.domain.errors.AppError
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.given
import mongo4cats.client.MongoClient
import mongo4cats.operations.Filter
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12353

  "SettingsRepository" when {
    "get" should {
      "return error settings do not exist" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            res  <- repo.get(Users.uid)
          yield res

          result.attempt.map(_ mustBe Left(AppError.NotSetup("Global")))
        }
      }
    }

    "update" should {
      "create new entry if settings do not exist" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.update(Settings.global)
            res  <- repo.get(Users.uid)
          yield res

          result.map(_ mustBe Settings.global)
        }
      }

      "update existing settings" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.update(Settings.global)
            _    <- repo.update(Settings.global.copy(trade = None, note = Some("update")))
            res  <- repo.get(Users.uid)
          yield res

          result.map(_ mustBe Settings.global.copy(trade = None, note = Some("update")))
        }
      }
    }

    "createFor" should {
      "create an entry in settings collection for a given user-id" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid)
            res  <- repo.get(Users.uid)
          yield res

          result.map(_.userId mustBe Users.uid)
        }
      }

      "not create a config if it already exists" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid)
            _    <- repo.createFor(Users.uid)
            res  <- db.getCollection("settings").flatMap(_.count(Filter.eq("userId", Users.uid.toObjectId)))
          yield res

          result.map(_ mustBe 1)
        }
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use { client =>
          for
            db  <- client.getDatabase("currexx")
            _   <- db.getCollection("settings").flatMap(_.insertOne(Document("userId" := Users.uid2.toObjectId)))
            res <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
