package currexx.core.trade

import cats.Monad
import cats.effect.Async
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerParameters
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, PriceRange, TradeOrder}
import currexx.domain.user.UserId
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant

final private class TradeController[F[_]](
    private val service: TradeService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import TradeController.*

  private def getTradeSettings(using auth: Authenticator[F]) =
    getTradeSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getSettings(session.userId)
          .mapResponse(s => TradeSettingsView(s.strategy, s.broker, s.trading, s.comment))
      }

  private def updateTradeSettings(using auth: Authenticator[F]) =
    updateTradeSettingsEndpoint.withAuthenticatedSession
      .serverLogic { session => settings =>
        service
          .updateSettings(settings.toDomain(session.userId))
          .voidResponse
      }

  private def closeCurrentPositions(using auth: Authenticator[F]) =
    closeCurrentPositionsEndpoint.withAuthenticatedSession
      .serverLogic { session => maybeCp =>
        maybeCp
          .fold(service.closeOpenOrders(session.userId))(cp => service.closeOpenOrders(session.userId, cp))
          .voidResponse
      }

  private def getTradeOrders(using auth: Authenticator[F]) =
    getTradeOrdersEndpoint.withAuthenticatedSession
      .serverLogic { session => sp =>
        (sp.from, sp.to).mapN((f, t) => F.raiseWhen(f.isAfter(t))(AppError.InvalidDateRange)).getOrElse(F.unit) >>
          service
            .getAllOrders(session.userId, sp)
            .mapResponse(_.map(TradeOrderView.from))
      }

  private def submitTradeOrderPlacement(using auth: Authenticator[F]) =
    submitTradeOrderPlacementEndpoint.withAuthenticatedSession
      .serverLogic { session => (order, closePendingOrders) =>
        service
          .placeOrder(session.userId, order, closePendingOrders)
          .voidResponse
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        getTradeSettings,
        updateTradeSettings,
        closeCurrentPositions,
        getTradeOrders,
        submitTradeOrderPlacement
      )
    )
}

object TradeController extends TapirSchema with TapirJson with TapirCodecs {

  final case class TradeSettingsView(
      strategy: TradeStrategy,
      broker: BrokerParameters,
      trading: TradingParameters,
      comment: Option[String]
  ) derives Codec.AsObject:
    def toDomain(userId: UserId): TradeSettings = TradeSettings(userId, strategy, broker, trading, comment)

  final case class TradeOrderView(
      order: TradeOrder,
      broker: BrokerParameters,
      time: Instant
  ) derives Codec.AsObject

  object TradeOrderView:
    def from(top: TradeOrderPlacement): TradeOrderView =
      TradeOrderView(top.order, top.broker, top.time)

  private val basePath     = "trade"
  private val settingsPath = basePath / "settings"
  private val ordersPath   = basePath / "orders"

  val getTradeOrdersEndpoint = Controller.securedEndpoint.get
    .in(ordersPath)
    .in(Controller.searchParams)
    .out(jsonBody[List[TradeOrderView]])
    .description("Retrieve placed trade orders")

  val submitTradeOrderPlacementEndpoint = Controller.securedEndpoint.post
    .in(ordersPath)
    .in(jsonBody[TradeOrder])
    .in(query[Boolean]("closePendingOrders").description("Close pending orders").default(true))
    .out(statusCode(StatusCode.Created))
    .description("Submit trade order placement")

  val closeCurrentPositionsEndpoint = Controller.securedEndpoint.delete
    .in(ordersPath)
    .in(query[Option[CurrencyPair]]("currencyPair"))
    .out(statusCode(StatusCode.NoContent))
    .description("Close all current positions")

  val getTradeSettingsEndpoint = Controller.securedEndpoint.get
    .in(settingsPath)
    .out(jsonBody[TradeSettingsView])
    .description("Retrieve settings for broker and trading")

  val updateTradeSettingsEndpoint = Controller.securedEndpoint.put
    .in(settingsPath)
    .in(jsonBody[TradeSettingsView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update settings for broker and trading")

  def make[F[_]: Async](service: TradeService[F]): F[Controller[F]] =
    Monad[F].pure(TradeController[F](service))
}
