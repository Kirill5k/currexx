package currexx.core.trade.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.common.http.SearchParams
import currexx.core.fixtures.{Markets, Trades, Users}
import currexx.core.trade.{CurrencyStatistics, EnterOrderStats, OrderStatistics}
import currexx.domain.market.{OrderPlacementStatus, TradeOrder}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class OrderStatusRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12357

  val emptySearchParams = SearchParams()

  "An OrderStatusRepository" when {
    "save" should {
      "store order status in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          res  <- repo.getStatistics(Users.uid, emptySearchParams)
        yield res

        result.map(_.totalOrders mustBe 1)
      }
    }

    "getStatistics" should {
      "return empty statistics when there are no orders" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- OrderStatusRepository.make(db)
          res  <- repo.getStatistics(Users.uid, emptySearchParams)
        yield res

        result.map { stats =>
          stats.totalOrders mustBe 0
          stats.successfulOrders mustBe 0
          stats.pendingOrders mustBe 0
          stats.cancelledOrders mustBe 0
          stats.noPositionOrders mustBe 0
          stats.enterOrders.total mustBe 0
          stats.exitOrders mustBe 0
          stats.currencyBreakdown mustBe Nil
        }
      }

      "not return statistics for orders belonging to a different user" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          res  <- repo.getStatistics(Users.uid2, emptySearchParams)
        yield res

        result.map(_.totalOrders mustBe 0)
      }

      "filter orders by placement time" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          _    <- repo.save(Trades.order.copy(time = Trades.ts.minusSeconds(200)), OrderPlacementStatus.Success)
          sp = SearchParams(Some(Trades.ts.minusSeconds(10)), Some(Trades.ts.plusSeconds(10)))
          res <- repo.getStatistics(Users.uid, sp)
        yield res

        result.map(_.totalOrders mustBe 1)
      }

      "return correct statistics for a single successful enter buy order" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          res  <- repo.getStatistics(Users.uid, emptySearchParams)
        yield res

        result.map { stats =>
          stats mustBe OrderStatistics(
            totalOrders = 1,
            successfulOrders = 1,
            pendingOrders = 0,
            cancelledOrders = 0,
            noPositionOrders = 0,
            enterOrders = EnterOrderStats(
              total = 1,
              buyCount = 1,
              sellCount = 0,
              totalVolume = BigDecimal(0.1),
              averageVolume = Some(BigDecimal(0.1))
            ),
            exitOrders = 0,
            currencyBreakdown = List(
              CurrencyStatistics(
                currencyPair = Markets.gbpeur,
                totalOrders = 1,
                successfulOrders = 1,
                pendingOrders = 0,
                cancelledOrders = 0,
                noPositionOrders = 0,
                enterOrders = 1,
                exitOrders = 0,
                buyOrders = 1,
                sellOrders = 0,
                totalVolume = BigDecimal(0.1)
              )
            )
          )
        }
      }

      "aggregate statistics correctly across multiple orders with various statuses" in withEmbeddedMongoDb { db =>
        val sellOrder = Trades.order.copy(order = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpusd, 5, BigDecimal(0.2)))
        val exitOrder = Trades.order.copy(order = TradeOrder.Exit(Markets.gbpeur, Markets.priceRange.close))

        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          _    <- repo.save(sellOrder, OrderPlacementStatus.Pending)
          _    <- repo.save(exitOrder, OrderPlacementStatus.Cancelled("timeout"))
          res  <- repo.getStatistics(Users.uid, emptySearchParams)
        yield res

        result.map { stats =>
          stats.totalOrders mustBe 3
          stats.successfulOrders mustBe 1
          stats.pendingOrders mustBe 1
          stats.cancelledOrders mustBe 1
          stats.enterOrders.total mustBe 2
          stats.enterOrders.buyCount mustBe 1
          stats.enterOrders.sellCount mustBe 1
          stats.enterOrders.totalVolume mustBe BigDecimal(0.1) + BigDecimal(0.2)
          stats.enterOrders.averageVolume mustBe Some((BigDecimal(0.1) + BigDecimal(0.2)) / 2)
          stats.exitOrders mustBe 1
          stats.currencyBreakdown.size mustBe 2
        }
      }

      "include currency breakdown with correct per-pair statistics" in withEmbeddedMongoDb { db =>
        val sellOrder = Trades.order.copy(order = TradeOrder.Enter(TradeOrder.Position.Sell, Markets.gbpusd, 5, BigDecimal(0.2)))

        val result = for
          repo <- OrderStatusRepository.make(db)
          _    <- repo.save(Trades.order, OrderPlacementStatus.Success)
          _    <- repo.save(sellOrder, OrderPlacementStatus.Pending)
          res  <- repo.getStatistics(Users.uid, emptySearchParams)
        yield res

        result.map { stats =>
          val gbpeurStats = stats.currencyBreakdown.find(_.currencyPair == Markets.gbpeur)
          val gbpusdStats = stats.currencyBreakdown.find(_.currencyPair == Markets.gbpusd)

          gbpeurStats mustBe Some(
            CurrencyStatistics(
              currencyPair = Markets.gbpeur,
              totalOrders = 1,
              successfulOrders = 1,
              pendingOrders = 0,
              cancelledOrders = 0,
              noPositionOrders = 0,
              enterOrders = 1,
              exitOrders = 0,
              buyOrders = 1,
              sellOrders = 0,
              totalVolume = BigDecimal(0.1)
            )
          )

          gbpusdStats mustBe Some(
            CurrencyStatistics(
              currencyPair = Markets.gbpusd,
              totalOrders = 1,
              successfulOrders = 0,
              pendingOrders = 1,
              cancelledOrders = 0,
              noPositionOrders = 0,
              enterOrders = 1,
              exitOrders = 0,
              buyOrders = 0,
              sellOrders = 1,
              totalVolume = BigDecimal(0.2)
            )
          )
        }
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

