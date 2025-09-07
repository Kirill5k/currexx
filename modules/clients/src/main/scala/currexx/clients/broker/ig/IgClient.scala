package currexx.clients.broker.ig

import cats.data.NonEmptyList
import cats.effect.Async
import cats.syntax.flatMap.*
import io.circe.Codec
import currexx.clients.Fs2HttpClient
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.{Currency, CurrencyPair, OpenedTradeOrder, TradeOrder}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend

import java.util.UUID

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
        case Right(res)  => F.pure(res)
        case Left(error) => handleError(error)
    }

  private def openPosition(login: IgClient.LoginResponse, params: BrokerParameters.Ig, order: TradeOrder.Enter): F[Unit] =
    dispatch(
      basicRequest
        .header("Version", "2")
        .header("X-IG-API-KEY", params.apiKey)
        .header("IG-ACCOUNT-ID", login.accountId)
        .auth
        .bearer(login.oauthToken.access_token)
        .body(asJson(IgClient.OpenPositionRequest.from(params.currency, order)))
        .response(asJson[IgClient.OpenPositionResponse])
        .post(uri"${baseUrl(params.demo)}")
    ).flatMap { r =>
      r.body match
        case Right(_)    => F.unit
        case Left(error) => handleError(error)
    }

  private def handleError[A](error: ResponseException[String]): F[A] =
    error match {
      case ResponseException.DeserializationException(responseBody, error, _) =>
        logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
          F.raiseError(AppError.JsonParsingFailure(responseBody, s"${name} client returned $error"))
      case ResponseException.UnexpectedStatusCode(body, meta) =>
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

  final case class OpenPositionRequest(
      currencyCode: String,
      dealReference: String,
      direction: String,
      epic: String,
      expiry: String,
      forceOpen: Boolean,
      guaranteedStop: Boolean,
      orderType: String,
      size: Double
  ) derives Codec.AsObject

  object OpenPositionRequest:
    def from(currency: Currency, order: TradeOrder.Enter): OpenPositionRequest =
      OpenPositionRequest(
        currencyCode = currency.code,
        dealReference = UUID.randomUUID().toString,
        direction = order.position.print.toUpperCase,
        epic = s"CS.D.${order.currencyPair}.TODAY.IP",
        expiry = "DFB",
        forceOpen = false,
        guaranteedStop = false,
        orderType = "MARKET",
        size = order.volume.toDouble
      )

  final case class OpenPositionResponse(dealReference: String) derives Codec.AsObject
}
