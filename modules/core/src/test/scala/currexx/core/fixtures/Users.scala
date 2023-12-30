package currexx.core.fixtures

import currexx.domain.user.{Password, PasswordHash, User, UserDetails, UserEmail, UserId, UserName}
import mongo4cats.bson.ObjectId

import java.time.Instant
import java.time.temporal.ChronoField

object Users {
  lazy val uid  = UserId(ObjectId().toHexString)
  lazy val uid2 = UserId(ObjectId().toHexString)

  lazy val regDate = Instant.now.`with`(ChronoField.MILLI_OF_SECOND, 0)
  lazy val pwd     = Password("password")
  lazy val hash    = PasswordHash("hash")
  lazy val email   = UserEmail("acc1@et.com")
  lazy val details = UserDetails(email, UserName("John", "Bloggs"))
  lazy val user    = User(uid, details.email, details.name, hash, regDate)
}
