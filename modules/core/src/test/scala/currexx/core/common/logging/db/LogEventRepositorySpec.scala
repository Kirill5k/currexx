package currexx.core.common.logging.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.common.logging.{LogEvent, LogLevel}
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import java.time.Instant
import java.time.temporal.ChronoField
import scala.concurrent.Future

class LogEventRepositorySpec extends MongoSpec {
  override protected val mongoPort: Int = 12352

  val ts = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)

  "LogEventRepository" when {
    "save" should {
      "store log-event in the repository" in withEmbeddedMongoDb { db =>
        val result = for
          repo <- LogEventRepository.make(db)
          _    <- repo.save(LogEvent(LogLevel.Error, ts, "uh-oh!"))
          res  <- repo.getAll
        yield res

        result.map(_ mustBe List(LogEvent(LogLevel.Error, ts, "uh-oh!")))
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
