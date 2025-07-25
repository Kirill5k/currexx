package currexx.core.trade.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Trades, Users}
import currexx.domain.market.TradeOrder
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class TradeOrderRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12355

  val emptySearchParams = SearchParams()

  "A TradeOrderRepository" when {
    "save" should {
      "store order in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          _    <- repo.save(Trades.order.copy(time = Trades.ts.minusSeconds(100)))
          res  <- repo.getAll(Users.uid, emptySearchParams)
        yield res

        result.map(_ mustBe List(Trades.order, Trades.order.copy(time = Trades.ts.minusSeconds(100))))
      }
    }

    "getAll" should {
      "not return anything when there are no orders for provided user-id" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          res  <- repo.getAll(Users.uid2, emptySearchParams)
        yield res

        result.map(_ mustBe Nil)
      }

      "filter orders by placement time" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          sp = SearchParams(Some(Trades.ts.minusSeconds(10)), Some(Trades.ts.plusSeconds(10)))
          res <- repo.getAll(Users.uid, sp)
        yield res

        result.map(_ mustBe List(Trades.order))
      }
    }

    "findLatestBy" should {
      "return latest order" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          _    <- repo.save(Trades.order.copy(time = Trades.ts.minusSeconds(100)))
          res  <- repo.findLatestBy(Users.uid, Markets.gbpeur)
        yield res

        result.map(_ mustBe Some(Trades.order))
      }

      "return empty option when there are no orders" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          res  <- repo.findLatestBy(Users.uid, Markets.gbpeur)
        yield res

        result.map(_ mustBe None)
      }
    }

    "getAllTradedCurrencies" should {
      "return all traded currencies" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- TradeOrderRepository.make(db)
          _    <- repo.save(Trades.order)
          _    <- repo.save(Trades.order.copy(order = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpusd, 5, 0.1)))
          res  <- repo.getAllTradedCurrencies(Users.uid)
        yield res

        result.map(_ mustBe List(Markets.gbpeur, Markets.gbpusd))
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
