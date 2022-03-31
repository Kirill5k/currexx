package currexx.core.fixtures

import currexx.core.auth.session.{CreateSession, IpAddress, Session, SessionId, SessionStatus}
import currexx.core.auth.user.UserId
import mongo4cats.bson.ObjectId

import java.time.Instant
import java.time.temporal.ChronoField

object Sessions {
  lazy val sid  = SessionId(ObjectId().toHexString)
  lazy val sid2 = SessionId(ObjectId().toHexString)
  lazy val ts   = Instant.now().`with`(ChronoField.MILLI_OF_SECOND, 0)
  lazy val ip   = IpAddress("localhost", 8080)

  lazy val sess = Session(sid, Users.uid, ts, true, SessionStatus.Authenticated, Some(ip), None)

  def create(
      uid: UserId = Users.uid,
      ip: Option[IpAddress] = Some(ip),
      ts: Instant = ts
  ): CreateSession = CreateSession(uid, ip, ts)
}
