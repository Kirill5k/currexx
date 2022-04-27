package currexx.core.market.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.clients.broker.BrokerParameters
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Users}
import currexx.domain.errors.AppError
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class MarketSettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12350

  "A MarketSettingsRepository" when {
    "update" should {
      "create new market-settings in a repository if it is new" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketSettingsRepository.make(db)
          _    <- repo.update(Markets.settings)
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Markets.settings)
      }

      "return error when market-settings do not exisst" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketSettingsRepository.make(db)
          res  <- repo.get(Users.uid)
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Market")))
      }

      "update existing market-settings" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketSettingsRepository.make(db)
          _    <- repo.update(Markets.settings)
          _    <- repo.update(Markets.settings.copy(broker = BrokerParameters.Vindaloo("2")))
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Markets.settings.copy(broker = BrokerParameters.Vindaloo("2")))
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://$mongoHost:$mongoPort")
        .use { client =>
          for
            db  <- client.getDatabase("currexx")
            res <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
