package currexx.core.trade.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.clients.broker.BrokerParameters
import currexx.core.MongoSpec
import currexx.core.fixtures.{Trades, Users}
import currexx.core.trade.db.TradeSettingsRepository
import currexx.core.trade.TradeStrategy
import currexx.domain.errors.AppError
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class TradeSettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12350

  "A TradeSettingsRepository" when {
    "update" should {
      "create new market-settings in a repository if it is new" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          _    <- repo.update(Trades.settings)
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Trades.settings)
      }

      "return error when market-settings do not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          res  <- repo.get(Users.uid)
        yield res

        result.attempt.map(_ mustBe Left(AppError.NotSetup("Trade")))
      }

      "update existing market-settings" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeSettingsRepository.make(db)
          _    <- repo.update(Trades.settings)
          _    <- repo.update(Trades.settings.copy(broker = BrokerParameters.Vindaloo("2"), strategy = TradeStrategy.TrendChange))
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Trades.settings.copy(broker = BrokerParameters.Vindaloo("2"), strategy = TradeStrategy.TrendChange))
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://$mongoHost:$mongoPort")
        .use { client =>
          client.getDatabase("currexx").flatMap(test)
        }
    }.unsafeToFuture()(IORuntime.global)
}
