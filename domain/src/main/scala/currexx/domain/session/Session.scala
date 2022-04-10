package currexx.domain.session

import currexx.domain.types.IdType
import currexx.domain.user.UserId
import io.circe.{Decoder, Encoder}

import java.time.Instant
import scala.util.Try

opaque type SessionId = String
object SessionId extends IdType[SessionId]

enum SessionStatus(val value: String):
  case Authenticated extends SessionStatus("authenticated")
  case LoggedOut     extends SessionStatus("logged-out")
  case Invalidated   extends SessionStatus("invalidated")

object SessionStatus {
  def from(value: String): Either[String, SessionStatus] =
    SessionStatus.values.find(_.value == value).toRight(s"Unexpected session status $value")

  given Decoder[SessionStatus] = Decoder[String].emap(SessionStatus.from)
  given Encoder[SessionStatus] = Encoder[String].contramap(_.value)
}

final case class IpAddress(host: String, port: Int)
object IpAddress {
  inline given Decoder[IpAddress] = Decoder[String].emapTry { ip =>
    Try(ip.split(":")).map(a => IpAddress(a.headOption.getOrElse("0.0.0.0"), a.drop(1).headOption.getOrElse("80").toInt))
  }
  inline given Encoder[IpAddress] = Encoder[String].contramap(ip => s"${ip.host}:${ip.port}")
}

final case class Session(
    id: SessionId,
    userId: UserId,
    createdAt: Instant,
    active: Boolean,
    status: SessionStatus,
    ipAddress: Option[IpAddress],
    lastAccessedAt: Option[Instant]
)

final case class CreateSession(
    userId: UserId,
    ipAddress: Option[IpAddress],
    time: Instant
)
