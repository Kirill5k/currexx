package currexx.core.market.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Users}
import currexx.core.market.MarketState
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class MarketStateRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12351

  "A MarketStateRepository" when {
    "find" should {
      "return empty option when state does not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          res  <- repo.find(Users.uid, Markets.gbpusd)
        yield res

        result.map(_ mustBe None)
      }

      "return state by currency" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Markets.priceRange)
          res  <- repo.find(Users.uid, Markets.gbpeur)
        yield res

        result.map(_ mustBe Some(MarketState(Users.uid, Markets.gbpeur, None, Some(Markets.priceRange), None)))
      }
    }

    "getAll" should {
      "return all market currency states" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Markets.priceRange)
          _    <- repo.update(Users.uid, Markets.gbpusd, Markets.priceRange)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map {
          _ mustBe List(
            MarketState(Users.uid, Markets.gbpeur, None, Some(Markets.priceRange), None),
            MarketState(Users.uid, Markets.gbpusd, None, Some(Markets.priceRange), None)
          )
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
            db  <- client.getDatabase("currexx")
            res <- test(db)
          yield res
        }
    }.unsafeToFuture()(IORuntime.global)
}
