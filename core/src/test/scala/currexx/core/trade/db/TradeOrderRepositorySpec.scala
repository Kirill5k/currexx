package currexx.core.trade.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Trades, Users}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class TradeOrderRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12351

  "A TradeOrderRepository" when {
    "save" should {
      "store order in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          res  <- repo.getAll(Users.uid)
        yield res

        result.map(_ mustBe List(Trades.order))
      }
    }

    "getAll" should {
      "not return anything when there are no orders for provided user-id" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          res  <- repo.getAll(Users.uid2)
        yield res

        result.map(_ mustBe Nil)
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
