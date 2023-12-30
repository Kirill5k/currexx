package currexx.core.auth.session.db

import io.circe.Codec
import currexx.domain.user.UserId
import currexx.domain.session.{CreateSession, IpAddress, Session, SessionId, SessionStatus}
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class SessionEntity(
    _id: ObjectId,
    userId: ObjectId,
    createdAt: Instant,
    active: Boolean,
    status: SessionStatus,
    ipAddress: Option[IpAddress],
    lastAccessedAt: Option[Instant]
) derives Codec.AsObject {
  def toDomain: Session =
    Session(
      id = SessionId(_id),
      userId = UserId(userId),
      createdAt = createdAt,
      active = active,
      status = status,
      ipAddress = ipAddress,
      lastAccessedAt = lastAccessedAt
    )
}

object SessionEntity {
  def create(cs: CreateSession): SessionEntity =
    SessionEntity(
      ObjectId(),
      cs.userId.toObjectId,
      cs.time,
      true,
      SessionStatus.Authenticated,
      cs.ipAddress,
      None
    )
}
