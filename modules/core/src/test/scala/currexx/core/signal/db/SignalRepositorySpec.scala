package currexx.core.signal.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.signal.Direction
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SignalRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12348

  val emptySearchParams = SearchParams(None, None, None)

  "A SignalRepository" when {
    "save" should {
      "store signal in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.saveAll(List(Signals.trend(Direction.Upward)))
          res  <- repo.getAll(Users.uid, emptySearchParams)
        yield res

        result.map(_ mustBe List(Signals.trend(Direction.Upward)))
      }
    }

    "getAll" should {
      "return all signals sorted by time in descending order" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.saveAll(
            List(
              Signals.trend(Direction.Upward),
              Signals.trend(Direction.Upward, time = Signals.ts.minusSeconds(10))
            )
          )
          res <- repo.getAll(Users.uid, emptySearchParams)
        yield res

        result.map(_.head mustBe Signals.trend(Direction.Upward))
      }

      "not return anything when there are no signals for provided user-id" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.saveAll(List(Signals.trend(Direction.Upward)))
          res  <- repo.getAll(Users.uid2, emptySearchParams)
        yield res

        result.map(_ mustBe Nil)
      }

      "filter out signals by time" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.saveAll(
            List(
              Signals.trend(Direction.Upward),
              Signals.trend(Direction.Upward, time = Signals.ts.minusSeconds(10))
            )
          )
          sp = SearchParams(Some(Signals.ts.minusSeconds(100)), Some(Signals.ts.minusSeconds(50)), None)
          res <- repo.getAll(Users.uid, sp)
        yield res

        result.map(_ mustBe Nil)
      }

      "filter out signals by currencyPair" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- SignalRepository.make(db)
          _    <- repo.saveAll(List(Signals.trend(Direction.Upward)))
          sp = SearchParams(currencyPair = Some(Markets.gbpusd))
          res <- repo.getAll(Users.uid, sp)
        yield res

        result.map(_ mustBe Nil)
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use(_.getDatabase("currexx").flatMap(test))
    }.unsafeToFuture()(using IORuntime.global)
}
