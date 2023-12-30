package currexx.core.auth.user.db

import currexx.domain.user.{PasswordHash, User, UserDetails, UserEmail, UserId, UserName}
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class UserEntity(
    _id: ObjectId,
    email: String,
    name: UserName,
    password: String,
    registrationDate: Instant
) derives Codec.AsObject {
  def toDomain: User =
    User(
      id = UserId(_id),
      email = UserEmail(email),
      name = name,
      password = PasswordHash(password),
      registrationDate = registrationDate
    )
}

object UserEntity {
  def create(details: UserDetails, password: PasswordHash): UserEntity =
    UserEntity(
      ObjectId(),
      details.email.value,
      details.name,
      password.value,
      Instant.now()
    )
}
