package currexx.core.auth.user.db

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import currexx.domain.user.*
import currexx.domain.errors.AppError.{AccountAlreadyExists, EntityDoesNotExist}
import currexx.core.common.db.Repository
import currexx.core.common.db.Repository.Field
import mongo4cats.circe.MongoJsonCodecs
import mongo4cats.operations.{Filter, Index, Update}
import mongo4cats.collection.MongoCollection
import mongo4cats.database.MongoDatabase
import mongo4cats.models.collection.IndexOptions

trait UserRepository[F[_]] extends Repository[F]:
  def find(uid: UserId): F[User]
  def findBy(email: UserEmail): F[Option[User]]
  def create(details: UserDetails, password: PasswordHash): F[UserId]
  def updatePassword(uid: UserId)(password: PasswordHash): F[Unit]

final private class LiveUserRepository[F[_]](
    private val collection: MongoCollection[F, UserEntity]
)(using
    F: Async[F]
) extends UserRepository[F] {

  override def findBy(email: UserEmail): F[Option[User]] =
    collection
      .find(Filter.eq(Field.Email, email.value))
      .first
      .mapOption(_.toDomain)

  override def create(details: UserDetails, password: PasswordHash): F[UserId] =
    collection
      .count(Filter.eq(Field.Email, details.email.value))
      .flatMap {
        case 0 =>
          val createAcc = UserEntity.create(details, password)
          collection.insertOne(createAcc).as(UserId(createAcc._id.toHexString))
        case _ =>
          AccountAlreadyExists(details.email).raiseError[F, UserId]
      }

  override def find(uid: UserId): F[User] =
    collection
      .find(idEq(uid.value))
      .first
      .mapOption(_.toDomain)
      .flatMap(maybeUser => F.fromOption(maybeUser, EntityDoesNotExist("User", uid.value)))

  override def updatePassword(uid: UserId)(password: PasswordHash): F[Unit] =
    collection
      .updateOne(idEq(uid.value), Update.set("password", password.value))
      .flatMap(errorIfNoMatches(EntityDoesNotExist("User", uid.value)))
}

object UserRepository extends MongoJsonCodecs:
  val indexByEmail = Index.ascending(Field.Email)

  def make[F[_]: Async](db: MongoDatabase[F]): F[UserRepository[F]] =
    db.getCollectionWithCodec[UserEntity](Repository.Collection.Users)
      .flatTap(_.createIndex(indexByEmail, IndexOptions().unique(true)))
      .map(LiveUserRepository[F](_))
