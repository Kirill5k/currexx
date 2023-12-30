package currexx.core.trade

import cats.Monad
import cats.effect.Async
import cats.effect.syntax.spawn.*
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import currexx.clients.broker.BrokerParameters
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirCodecs, TapirJson, TapirSchema}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, TradeOrder}
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

  private def closeCurrentPositions(using auth: Authenticator[F]) =
    closeCurrentPositionsEndpoint.withAuthenticatedSession
      .serverLogic { session => maybeCp =>
        val process = maybeCp match
          case Some(cp) => service.closeOpenOrders(session.userId, cp)
          case None     => service.closeOpenOrders(session.userId)

        process.start.voidResponse
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
        closeCurrentPositions,
        getTradeOrders,
        submitTradeOrderPlacement
      )
    )
}

object TradeController extends TapirSchema with TapirJson with TapirCodecs {

  final case class TradeOrderView(
      order: TradeOrder,
      broker: BrokerParameters,
      time: Instant
  ) derives Codec.AsObject

  object TradeOrderView:
    def from(top: TradeOrderPlacement): TradeOrderView =
      TradeOrderView(top.order, top.broker, top.time)

  private val basePath   = "trade"
  private val ordersPath = basePath / "orders"

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

  def make[F[_]: Async](service: TradeService[F]): F[Controller[F]] =
    Monad[F].pure(TradeController[F](service))
}
