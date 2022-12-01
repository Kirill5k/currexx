package currexx.core.settings.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.syntax.given
import currexx.core.fixtures.{Settings, Users}
import currexx.core.MongoSpec
import currexx.core.settings.{GlobalSettings, SignalParameters, TradeParameters}
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
      "return empty option when settings do not exist" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            res  <- repo.get(Users.uid2)
          yield res

          result.attempt.map(_ mustBe Left(AppError.NotSetup("Global")))
        }
      }

      "join data from 2 tables and return single object" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            res  <- repo.get(Users.uid)
          yield res

          result.map { settings =>
            settings mustBe GlobalSettings(
              Users.uid,
              Some(SignalParameters(Settings.signal.triggerFrequency, Settings.signal.indicators)),
              Some(TradeParameters(Settings.trade.strategy, Settings.trade.broker, Settings.trade.trading, Settings.trade.comment))
            )
          }
        }
      }
    }

    "createFor" should {
      "create an entry in settings collection for a given user-id" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid2)
            res  <- repo.get(Users.uid2)
          yield res

          result.map(_.userId mustBe Users.uid2)
        }
      }

      "not create a config if it already exists" in {
        withEmbeddedMongoDb { db =>
          val result = for
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid2)
            _    <- repo.createFor(Users.uid2)
            res  <- db.getCollection("settings").flatMap(_.count(Filter.eq("userId", Users.uid2.toObjectId)))
          yield res

          result.map(_ mustBe 1)
        }
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://$mongoHost:$mongoPort")
        .use { client =>
          for
            db <- client.getDatabase("currexx")
            _  <- db.getCollection("settings").flatMap(_.insertOne(Document("userId" := Users.uid.toObjectId)))
            signal = Document(
              "userId"           := Users.uid.toObjectId,
              "triggerFrequency" := Settings.signal.triggerFrequency,
              "indicators"       := Settings.signal.indicators
            )
            _ <- db.getCollection("signal-settings").flatMap(_.insertOne(signal))
            trade = Document(
              "userId"   := Users.uid.toObjectId,
              "strategy" := Settings.trade.strategy,
              "broker"   := Settings.trade.broker,
              "trading"  := Settings.trade.trading,
              "comment"  := Settings.trade.comment
            )
            _   <- db.getCollection("trade-settings").flatMap(_.insertOne(trade))
            res <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
