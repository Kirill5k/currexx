package currexx.core.signal.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Signals, Users}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SignalRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12348

  "A SignalRepository" when {
    "save" should {
      "store signal in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.save(Signals.trendDirectionChanged)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_ mustBe List(Signals.trendDirectionChanged))
      }
    }

    "getAll" should {
      "return all signals sorted by time in descending order" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.save(Signals.trendDirectionChanged)
          _    <- repo.save(Signals.trendDirectionChanged.copy(time = Signals.ts.minusSeconds(10)))
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_.head mustBe Signals.trendDirectionChanged)
      }

      "not return anything when there are no signals for provided user-id" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.save(Signals.trendDirectionChanged)
          res  <- repo.getAll(Users.uid2)
        yield res

        result.map(_ mustBe Nil)
      }
    }

    "isFirstOfItsKindForThatDate" should {
      "return false if signal of such kind has been already submitted on the same date" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.save(Signals.trendDirectionChanged)
          res  <- repo.isFirstOfItsKindForThatDate(Signals.trendDirectionChanged)
        yield res

        result.map(_ mustBe false)
      }

      "return true if it is a first signal of such kind" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.save(Signals.trendDirectionChanged)
          res  <- repo.isFirstOfItsKindForThatDate(Signals.trendDirectionChanged.copy(currencyPair = Markets.gbpusd))
        yield res

        result.map(_ mustBe true)
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
