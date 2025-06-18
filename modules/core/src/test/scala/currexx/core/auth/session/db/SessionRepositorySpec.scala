package currexx.core.auth.session.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import cats.syntax.apply.*
import cats.syntax.option.*
import currexx.core.MongoSpec
import currexx.core.fixtures.{Sessions, Users}
import currexx.domain.session.*
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class SessionRepositorySpec extends MongoSpec {

  override protected val mongoPort: Int = 12347

  "A SessionRepository" should {
    "create new sessions" in {
      withEmbeddedMongoDb { db =>
        val result = for
          repo <- SessionRepository.make(db)
          sid  <- repo.create(Sessions.create())
          res  <- repo.find(sid)
        yield (sid, res)

        result.map { case (sid, sess) =>
          sess mustBe Session(
            sid,
            Users.uid,
            Sessions.ts,
            true,
            SessionStatus.Authenticated,
            Some(Sessions.ip),
            None
          ).some
        }
      }
    }

    "return empty option when session does not exist" in {
      withEmbeddedMongoDb { db =>
        val result = for
          repo <- SessionRepository.make(db)
          res  <- repo.find(Sessions.sid)
        yield res

        result.map(_ mustBe None)
      }
    }

    "unauth session" in {
      withEmbeddedMongoDb { db =>
        val result = for
          repo <- SessionRepository.make(db)
          sid  <- repo.create(Sessions.create())
          _    <- repo.unauth(sid)
          res  <- repo.find(sid)
        yield res

        result.map { s =>
          val sess = s.get
          sess.active mustBe false
          sess.status mustBe SessionStatus.LoggedOut
        }
      }
    }

    "invalidate all sessions" in {
      withEmbeddedMongoDb { db =>
        val result = for
          repo <- SessionRepository.make(db)
          sid1 <- repo.create(Sessions.create())
          sid2 <- repo.create(Sessions.create())
          _    <- repo.invalidatedAll(Users.uid)
          res  <- (repo.find(sid1), repo.find(sid2)).tupled
        yield res

        result.map {
          case (Some(s1), Some(s2)) =>
            s1.status mustBe SessionStatus.Invalidated
            s1.active mustBe false
            s2.status mustBe SessionStatus.Invalidated
            s2.active mustBe false
          case _ => fail("unexpected match")
        }
      }
    }

    "update session lastAccessedAt field on find" in {
      withEmbeddedMongoDb { db =>
        val result = for
          repo <- SessionRepository.make(db)
          sid  <- repo.create(Sessions.create())
          _    <- repo.find(sid)
          res  <- repo.find(sid)
        yield res

        result.map { sess =>
          sess.flatMap(_.lastAccessedAt) must not be empty
        }
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use { client =>
          client.getDatabase("currexx").flatMap(test)
        }
    }.unsafeToFuture()(using IORuntime.global)
}
