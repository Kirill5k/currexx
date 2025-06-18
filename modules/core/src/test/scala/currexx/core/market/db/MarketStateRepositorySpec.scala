package currexx.core.market.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Users}
import currexx.core.market.MarketState
import currexx.domain.errors.AppError
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class MarketStateRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12354

  "A MarketStateRepository" when {
    "update signals" should {
      "create new state with signals if it doesn't exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          res  <- repo.update(Users.uid, Markets.gbpeur, Markets.indicatorStates)
        yield res

        result.map(_.withoutCreatedAt mustBe MarketState(Users.uid, Markets.gbpeur, None, Markets.indicatorStates, None, None))
      }

      "update existing state if it exists" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Map.empty)
          _    <- repo.update(Users.uid, Markets.gbpeur, Markets.indicatorStates)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_ must have size 1)
      }
    }

    "update current position" should {
      "update position field in the state" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Markets.indicatorStates)
          res  <- repo.update(Users.uid, Markets.gbpeur, Some(Markets.positionState))
        yield res

        result.map(_.currentPosition mustBe Some(Markets.positionState))
      }
    }

    "find" should {
      "return empty option when state does not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          res  <- repo.find(Users.uid, Markets.gbpusd)
        yield res

        result.map(_ mustBe None)
      }
    }

    "getAll" should {
      "return all market currency states" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Map.empty)
          _    <- repo.update(Users.uid, Markets.gbpusd, Map.empty)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map {
          _.map(_.withoutCreatedAt) mustBe List(
            MarketState(Users.uid, Markets.gbpeur, None, Map.empty, None, None),
            MarketState(Users.uid, Markets.gbpusd, None, Map.empty, None, None)
          )
        }
      }
    }

    "deleteAll" should {
      "delete all market currency states" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Map.empty)
          _    <- repo.update(Users.uid, Markets.gbpusd, Map.empty)
          _    <- repo.deleteAll(Users.uid)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_ mustBe Nil)
      }
    }

    "delete" should {
      "delete market currency state" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.update(Users.uid, Markets.gbpeur, Map.empty)
          _    <- repo.delete(Users.uid, Markets.gbpeur)
          res  <- repo.find(Users.uid, Markets.gbpeur)
        yield res

        result.map(_ mustBe None)
      }

      "return error when market state does not exist" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- MarketStateRepository.make(db)
          _    <- repo.delete(Users.uid, Markets.gbpeur)
        yield ()

        result.attempt.map(_ mustBe Left(AppError.NotTracked(List(Markets.gbpeur))))
      }
    }
  }

  extension (s: MarketState) def withoutCreatedAt: MarketState = s.copy(createdAt = None)

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use(_.getDatabase("currexx").flatMap(test))
    }.unsafeToFuture()(using IORuntime.global)
}
