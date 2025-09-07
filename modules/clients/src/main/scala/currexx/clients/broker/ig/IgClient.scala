package currexx.clients.broker.ig

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.flatMap.*
import io.circe.Codec
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, TradeOrder}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend

private[clients] trait IgClient[F[_]] extends Fs2HttpClient[F]:
  def submit(params: BrokerParameters.Ig, order: TradeOrder): F[Unit]
  def getCurrentOrders(params: BrokerParameters.Ig, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveXtbClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: IgConfig
)(using
    F: Async[F],
    logger: Logger[F]
) extends IgClient[F] {

  override protected val name: String = "ig"

  override def submit(params: BrokerParameters.Ig, order: TradeOrder): F[Unit] = ???

  override def getCurrentOrders(params: BrokerParameters.Ig, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] = ???

  private def baseUrl(demo: Boolean): String =
    val base = if demo then config.baseUri.replaceAll("api", "demo-api") else config.baseUri
    s"${base}/gateway/deal"

  private def login(params: BrokerParameters.Ig): F[IgClient.LoginResponse] =
    dispatch(
      basicRequest
        .header("Version", "3")
        .header("X-IG-API-KEY", params.apiKey)
        .body(asJson(IgClient.LoginRequest(params.username, params.password)))
        .response(asJson[IgClient.LoginResponse])
        .post(uri"${baseUrl(params.demo)}")
    ).flatMap { r =>
      r.body match
        case Right(res) =>
          F.pure(res)
        case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
          logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
            F.raiseError(AppError.JsonParsingFailure(responseBody, s"${name} client returned $error"))
        case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
          logger.error(s"$name-client/${meta.code.code}\n$body") >>
            F.raiseError(AppError.ClientFailure(name, s"$name return ${meta.code}"))
    }
}

object IgClient {
  final case class LoginRequest(
      identifier: String,
      password: String
  ) derives Codec.AsObject

  final case class LoginResponse(
      accountId: String,
      oauthToken: OauthToken
  ) derives Codec.AsObject

  final case class OauthToken(
      access_token: String
  ) derives Codec.AsObject
}
