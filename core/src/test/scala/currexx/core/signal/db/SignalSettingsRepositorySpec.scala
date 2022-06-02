package currexx.core.signal.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.market.v2.{Indicator, ValueSource, ValueTransformation}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SignalSettingsRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12349

  "A SignalSettingsRepository" when {
    "update" should {
      "create new signal-settings in a repository if it is new" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalSettingsRepository.make(db)
          _    <- repo.update(Signals.settings)
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Some(Signals.settings))
      }

      "update existing signal-settings" in withEmbeddedMongoDb { db =>
        val ema = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.EMA(16))
        val result = for
          repo <- SignalSettingsRepository.make(db)
          _    <- repo.update(Signals.settings)
          _    <- repo.update(Signals.settings.copy(indicators = List(ema)))
          res  <- repo.get(Users.uid)
        yield res

        result.map(_ mustBe Some(Signals.settings.copy(indicators = List(ema))))
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
