package currexx.core.monitor.db

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.fixtures.{Markets, Monitors, Users}
import currexx.core.monitor.Monitor
import currexx.domain.errors.AppError
import currexx.domain.market.Interval
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future
import scala.concurrent.duration.*

class MonitorRepositorySpec extends MongoSpec {

  override protected val mongoPort: Int = 12349

  "A MonitorRepository" when {
    "create" should {
      "create new monitor" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.create(Monitors.createMarketData())
          res  <- repo.find(Users.uid, mon.id)
        yield (mon, res)

        result.map { (created, found) =>
          found mustBe Monitors.genMarketData(mid = found.id, lastQueriedAt = None)
          created mustBe Monitors.genMarketData(mid = created.id, lastQueriedAt = None)
        }
      }

      "return error when creating monitor for an existing currency pair" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          _    <- repo.create(Monitors.createMarketData())
          _    <- repo.create(Monitors.createMarketData(pairs = NonEmptyList.of(Markets.gbpeur, Markets.gbpusd)))
        yield ()

        result.attempt.map { res =>
          res mustBe Left(AppError.AlreadyBeingMonitored(Set(Markets.gbpeur)))
        }
      }
    }

    "find" should {
      "return error when monitor does not exist" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.find(Users.uid, Monitors.mid)
        yield mon

        result.attempt.map { res =>
          res mustBe Left(AppError.EntityDoesNotExist("Monitor", Monitors.mid.value))
        }
      }

      "return error when monitor belongs to someone else" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.create(Monitors.createMarketData())
          res  <- repo.find(Users.uid2, mon.id)
        yield res

        result.attempt.map { res =>
          res mustBe a[Left[AppError.EntityDoesNotExist, Monitor]]
        }
      }
    }

    "getAll" should {
      "return all monitors" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon1 <- repo.create(Monitors.createMarketData())
          mon2 <- repo.create(Monitors.createMarketData(pairs = NonEmptyList.of(Markets.gbpusd)))
          mons <- repo.getAll(Users.uid)
        yield (List(mon1, mon2), mons)

        result.map { (created, found) =>
          created mustBe found
        }
      }

      "return empty list when there are no monitors" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          _    <- repo.create(Monitors.createMarketData())
          mons <- repo.getAll(Users.uid2)
        yield mons

        result.map(_ mustBe Nil)
      }
    }

    "activate" should {
      "update monitor status" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.create(Monitors.createMarketData())
          _    <- repo.activate(Users.uid, mon.id, false)
          upd  <- repo.find(Users.uid, mon.id)
        yield upd

        result.map { res =>
          res.active mustBe false
        }
      }

      "allow updating status multiple times" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mid  <- repo.create(Monitors.createMarketData()).map(_.id)
          _    <- repo.activate(Users.uid, mid, false)
          _    <- repo.activate(Users.uid, mid, false)
          _    <- repo.activate(Users.uid, mid, false)
        yield ()

        result.map(_ mustBe ())
      }

      "return error when monitor does not exist" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          _    <- repo.activate(Users.uid, Monitors.mid, false)
        yield ()

        result.attempt.map { res =>
          res mustBe Left(AppError.EntityDoesNotExist("Monitor", Monitors.mid.value))
        }
      }
    }

    "delete" should {
      "delete monitor from db and return deleted mon back" in withEmbeddedMongoDb { client =>
        val result = for
          repo    <- MonitorRepository.make(client)
          mon     <- repo.create(Monitors.createMarketData())
          deleted <- repo.delete(Users.uid, mon.id)
          mons    <- repo.getAll(Users.uid)
        yield (deleted, mons)

        result.map { case (deleted, mons) =>
          mons mustBe Nil
          deleted.userId mustBe Users.uid
          deleted.currencyPairs mustBe NonEmptyList.of(Markets.gbpeur)
        }
      }

      "return error when monitor does not exist" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          _    <- repo.delete(Users.uid, Monitors.mid)
        yield ()

        result.attempt.map { res =>
          res mustBe Left(AppError.EntityDoesNotExist("Monitor", Monitors.mid.value))
        }
      }
    }

    "update" should {
      "update monitor in db and return previous monitor" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.create(Monitors.createMarketData())
          old <- repo.update(Monitors.marketData.copy(id = mon.id, interval = Interval.M1, currencyPairs = NonEmptyList.of(Markets.gbpusd)))
          upd <- repo.find(Users.uid, mon.id)
        yield (old, upd)

        result.map { case (oldMon, updMon) =>
          updMon.asInstanceOf[Monitor.MarketData].interval mustBe Interval.M1
          oldMon.asInstanceOf[Monitor.MarketData].interval mustBe Interval.H1
          updMon.currencyPairs mustBe NonEmptyList.of(Markets.gbpusd)
          oldMon.currencyPairs mustBe NonEmptyList.of(Markets.gbpeur)
        }
      }

      "not allow to update currency pair to the one that's already being monitored" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mon  <- repo.create(Monitors.createMarketData())
          _    <- repo.create(Monitors.createMarketData(pairs = NonEmptyList.of(Markets.gbpusd)))
          _    <- repo.update(Monitors.marketData.copy(id = mon.id, currencyPairs = NonEmptyList.of(Markets.gbpusd)))
        yield ()

        result.attempt.map { res =>
          res mustBe Left(AppError.AlreadyBeingMonitored(Set(Markets.gbpusd)))
        }
      }
    }

    "updateQueriedTimestamp" should {
      "update last queried at timestamp in price monitor schedule" in withEmbeddedMongoDb { client =>
        val result = for
          repo   <- MonitorRepository.make(client)
          mon    <- repo.create(Monitors.createMarketData())
          _      <- repo.updateQueriedTimestamp(Users.uid, mon.id)
          updMon <- repo.find(Users.uid, mon.id)
        yield updMon

        result.map { updMon =>
          updMon.lastQueriedAt mustBe defined
        }
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
