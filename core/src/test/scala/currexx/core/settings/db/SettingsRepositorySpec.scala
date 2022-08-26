package currexx.core.settings.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import io.circe.syntax.given
import currexx.core.fixtures.{Signals, Trades, Users}
import currexx.core.MongoSpec
import currexx.core.settings.{Settings, SignalParameters, TradeParameters}
import mongo4cats.bson.Document
import mongo4cats.bson.syntax.*
import mongo4cats.circe.given
import mongo4cats.client.MongoClient
import mongo4cats.collection.operations.Filter
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12353

  "SettingsRepository" when {
    "get" should {
      "return empty option when settings do not exist" in {
        withEmbeddedMongoDb { db =>
          val result = for {
            repo <- SettingsRepository.make[IO](db)
            res  <- repo.get(Users.uid2)
          } yield res

          result.map(_ mustBe None)
        }
      }

      "join data from 2 tables and return single object" in {
        withEmbeddedMongoDb { db =>
          val result = for {
            repo <- SettingsRepository.make[IO](db)
            res  <- repo.get(Users.uid)
          } yield res.get

          result.map { settings =>
            settings mustBe Settings(
              Users.uid,
              Some(SignalParameters(Signals.settings.triggerFrequency, Signals.settings.indicators)),
              Some(TradeParameters(Trades.settings.strategy, Trades.settings.broker, Trades.settings.trading, Trades.settings.comment))
            )
          }
        }
      }
    }

    "createFor" should {
      "create an entry in settings collection for a given user-id" in {
        withEmbeddedMongoDb { db =>
          val result = for {
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid2)
            res  <- repo.get(Users.uid2)
          } yield res

          result.map(_.map(_.userId) mustBe Some(Users.uid2))
        }
      }

      "not create a config if it already exists" in {
        withEmbeddedMongoDb { db =>
          val result = for {
            repo <- SettingsRepository.make[IO](db)
            _    <- repo.createFor(Users.uid2)
            _    <- repo.createFor(Users.uid2)
            res  <- db.getCollection("settings").flatMap(_.count(Filter.eq("userId", Users.uid2.toObjectId)))
          } yield res

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
              "triggerFrequency" := Signals.settings.triggerFrequency,
              "indicators"       := Signals.settings.indicators
            )
            _ <- db.getCollection("signal-settings").flatMap(_.insertOne(signal))
            trade = Document(
              "userId"   := Users.uid.toObjectId,
              "strategy" := Trades.settings.strategy,
              "broker"   := Trades.settings.broker,
              "trading"  := Trades.settings.trading,
              "comment"  := Trades.settings.comment
            )
            _   <- db.getCollection("trade-settings").flatMap(_.insertOne(trade))
            res <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
