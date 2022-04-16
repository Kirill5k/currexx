package currexx.core.monitor.db

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
          mid  <- repo.create(Monitors.create())
          mon  <- repo.find(Users.uid, mid)
        yield (mid, mon)

        result.map { (mid, mon) =>
          mon mustBe Monitor(mid, Users.uid, true, Markets.gbpeur, Interval.H1, 3.hours, None)
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
          mid  <- repo.create(Monitors.create())
          mon  <- repo.find(Users.uid2, mid)
        yield mon

        result.attempt.map { res =>
          res mustBe a[Left[AppError.EntityDoesNotExist, Monitor]]
        }
      }
    }

    "getAll" should {
      "return all monitors" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mid1 <- repo.create(Monitors.create())
          mid2 <- repo.create(Monitors.create())
          mons <- repo.getAll(Users.uid)
        yield (List(mid1, mid2), mons)

        result.map { (ids, mons) =>
          ids mustBe mons.map(_.id)
        }
      }

      "return empty list when there are no monitors" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          _    <- repo.create(Monitors.create())
          mons <- repo.getAll(Users.uid2)
        yield mons

        result.map(_ mustBe Nil)
      }
    }

    "activate" should {
      "update monitor status" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mid  <- repo.create(Monitors.create())
          _    <- repo.activate(Users.uid, mid, false)
          mon  <- repo.find(Users.uid, mid)
        yield mon

        result.map { res =>
          res.active mustBe false
        }
      }

      "allow updating status multiple times" in withEmbeddedMongoDb { client =>
        val result = for
          repo <- MonitorRepository.make(client)
          mid  <- repo.create(Monitors.create())
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