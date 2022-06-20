package currexx.clients.broker.xtb

import io.circe.Codec

sealed trait RequestArguments
object RequestArguments:
  final case class Login(
      userId: String,
      password: String
  ) extends RequestArguments
      derives Codec.AsObject
  final case class Trade(
      cmd: Int,
      `type`: Int,
      symbol: String,
      volume: BigDecimal,
      price: BigDecimal,
      sl: BigDecimal,
      tp: BigDecimal,
      offset: BigDecimal,
      customComment: String
  ) extends RequestArguments
      derives Codec.AsObject

final case class XtbRequest[A <: RequestArguments](
    command: String,
    arguments: A
) derives Codec.AsObject

object XtbRequest {
  def login(userId: String, password: String): XtbRequest[RequestArguments.Login] =
    XtbRequest("login", RequestArguments.Login(userId, password))
}
