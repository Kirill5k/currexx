package currexx.core.auth.user.db

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import currexx.core.MongoSpec
import currexx.core.common.db.Repository
import currexx.domain.user.{PasswordHash, User, UserEmail, UserId}
import currexx.domain.errors.AppError.{AccountAlreadyExists, EntityDoesNotExist}
import currexx.core.fixtures.Users
import mongo4cats.bson.ObjectId
import mongo4cats.client.MongoClient
import mongo4cats.database.MongoDatabase

import scala.concurrent.Future

class UserRepositorySpec extends MongoSpec {

  override protected val mongoPort: Int = 12346

  "An UserRepository" when {

    "find" should {
      "find account by id" in {
        withEmbeddedMongoDb { client =>
          val result = for
            repo <- UserRepository.make(client)
            acc  <- repo.find(Users.uid)
          yield acc

          result.map { acc =>
            acc mustBe User(Users.uid, Users.details.email, Users.details.name, Users.hash, Users.regDate)
          }
        }
      }

      "return error account does not exist" in {
        withEmbeddedMongoDb { client =>
          val result = for
            repo <- UserRepository.make(client)
            acc  <- repo.find(Users.uid2)
          yield acc

          result.attempt.map(_ mustBe Left(EntityDoesNotExist("User", Users.uid2.value)))
        }
      }
    }

    "findBy" should {
      "find account by email" in {
        withEmbeddedMongoDb { client =>
          val result = for
            repo <- UserRepository.make(client)
            acc  <- repo.findBy(Users.details.email)
          yield acc

          result.map { acc =>
            acc mustBe Some(User(Users.uid, Users.details.email, Users.details.name, Users.hash, Users.regDate))
          }
        }
      }

      "return empty option when account does not exist" in {
        withEmbeddedMongoDb { client =>
          val result = for
            repo <- UserRepository.make(client)
            acc  <- repo.findBy(UserEmail("acc2@et.com"))
          yield acc

          result.map(_ mustBe None)
        }
      }
    }

    "updatePassword" should {
      "update account password" in {
        withEmbeddedMongoDb { client =>
          val newpwd = PasswordHash("new-password")
          val result = for
            repo <- UserRepository.make(client)
            _    <- repo.updatePassword(Users.uid)(newpwd)
            acc  <- repo.find(Users.uid)
          yield acc

          result.map { acc =>
            acc.password mustBe newpwd
          }
        }
      }

      "return error when account does not exist" in {
        withEmbeddedMongoDb { client =>
          val id = UserId(ObjectId().toHexString)
          val result = for
            repo <- UserRepository.make(client)
            acc  <- repo.updatePassword(id)(Users.hash)
          yield acc

          result.attempt.map(_ mustBe Left(EntityDoesNotExist("User", id.value)))
        }
      }
    }

    "create" should {
      "create new account" in {
        withEmbeddedMongoDb { client =>
          val email = UserEmail("acc2@et.com")

          val result = for
            repo <- UserRepository.make(client)
            aid  <- repo.create(Users.details.copy(email = email), Users.hash)
            acc  <- repo.findBy(email)
          yield (aid, acc)

          result.map {
            case (aid, Some(acc)) =>
              acc mustBe User(aid, email, Users.details.name, Users.hash, acc.registrationDate)
            case _ => fail("unmatched case")
          }
        }
      }

      "return error when account already exists" in {
        withEmbeddedMongoDb { client =>
          val result = for
            repo <- UserRepository.make(client)
            _    <- repo.create(Users.details, Users.hash)
          yield ()

          result.attempt.map(_ mustBe Left(AccountAlreadyExists(Users.details.email)))
        }
      }
    }
  }

  def withEmbeddedMongoDb[A](test: MongoDatabase[IO] => IO[A]): Future[A] =
    withRunningEmbeddedMongo {
      MongoClient
        .fromConnectionString[IO](s"mongodb://localhost:$mongoPort")
        .use { client =>
          for
            db   <- client.getDatabase("currexx")
            accs <- db.getCollection(Repository.Collection.Users)
            _    <- accs.insertOne(accDoc(Users.uid, Users.details.email, password = Users.hash, registrationDate = Users.regDate))
            res  <- test(db)
          yield res
        }
    }.unsafeToFuture()(using IORuntime.global)
}
