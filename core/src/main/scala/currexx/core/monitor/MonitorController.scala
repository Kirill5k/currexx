package currexx.core.monitor

import cats.Monad
import cats.data.NonEmptySet
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import io.circe.Codec
import org.http4s.HttpRoutes
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

final private class MonitorController[F[_]](
    private val service: MonitorService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import MonitorController.*

  private def setupNewMonitor(using authenticator: Authenticator[F]) =
    setupNewMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => req =>
        F.fromEither(req.toDomain(session.userId))
          .flatMap(service.create)
          .mapResponse(mid => CreateMonitorResponse(mid))
      }

  private def pauseMonitor(using authenticator: Authenticator[F]) =
    pauseMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .pause(session.userId, mid)
          .voidResponse
      }

  private def resumeMonitor(using authenticator: Authenticator[F]) =
    resumeMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .resume(session.userId, mid)
          .voidResponse
      }

  private def deleteMonitor(using authenticator: Authenticator[F]) =
    deleteMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .delete(session.userId, mid)
          .voidResponse
      }

  private def queryMonitor(using authenticator: Authenticator[F]) =
    queryMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .triggerPriceMonitor(session.userId, mid, true)
          .voidResponse
      }

  private def getMonitorById(using authenticator: Authenticator[F]) =
    getMonitorByIdEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .get(session.userId, mid)
          .mapResponse(MonitorView.from)
      }

  private def updateMonitor(using authenticator: Authenticator[F]) =
    updateMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => (mid, mon) =>
        F.raiseWhen(mid.value != mon.id)(AppError.IdMismatch) >>
          F.fromEither(mon.toDomain(session.userId))
            .flatMap(service.update)
            .voidResponse
      }

  private def compoundUpdateMonitor(using authenticator: Authenticator[F]) =
    compoundUpdateMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => req =>
        for
          _        <- F.fromEither(req.validated)
          monitors <- service.getAll(session.userId)
          updatedPairs = req.currencyPairs
          trackedPairs = monitors.map(_.currencyPair).toSet
          _ <- F.raiseWhen(!updatedPairs.subsetOf(trackedPairs))(AppError.NotTracked(updatedPairs.diff(trackedPairs)))
          res <- monitors
            .filter(m => updatedPairs(m.currencyPair))
            .map(_.copy(profit = req.profit, price = req.price, active = req.active))
            .traverse(service.update)
            .voidResponse
        yield res
      }

  private def getAllMonitors(using authenticator: Authenticator[F]) =
    getAllMonitorsEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        service
          .getAll(session.userId)
          .mapResponse(_.map(MonitorView.from))
      }

  override def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        compoundUpdateMonitor,
        setupNewMonitor,
        pauseMonitor,
        resumeMonitor,
        deleteMonitor,
        getMonitorById,
        updateMonitor,
        getAllMonitors,
        queryMonitor
      )
    )
}

object MonitorController extends TapirSchema with TapirJson {

  private def validate(pms: ProfitMonitorSchedule): Either[AppError, Unit] =
    Either.cond(
      pms.min.isDefined || pms.max.isDefined,
      (),
      AppError.FailedValidation("Profit monitor schedule needs to have min or max boundary specified")
    )

  final case class CompoundUpdateMonitorRequest(
      currencyPairs: Set[CurrencyPair],
      active: Boolean,
      price: PriceMonitorSchedule,
      profit: Option[ProfitMonitorSchedule]
  ) derives Codec.AsObject:
    def validated: Either[AppError, Unit] =
      for
        _ <- profit.map(validate).getOrElse(Right(()))
        _ <- Either.cond(currencyPairs.nonEmpty, (), AppError.FailedValidation("Currency pairs must not be empty"))
      yield ()

  final case class CreateMonitorRequest(
      currencyPair: CurrencyPair,
      price: PriceMonitorSchedule,
      profit: Option[ProfitMonitorSchedule]
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): Either[AppError, CreateMonitor] =
      for _ <- profit.map(validate).getOrElse(Right(()))
      yield CreateMonitor(uid, currencyPair, price, profit)

  final case class CreateMonitorResponse(id: MonitorId) derives Codec.AsObject

  final case class MonitorView(
      id: String,
      active: Boolean,
      currencyPair: CurrencyPair,
      price: PriceMonitorSchedule,
      profit: Option[ProfitMonitorSchedule]
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): Either[AppError, Monitor] =
      for _ <- profit.map(validate).getOrElse(Right(()))
      yield Monitor(MonitorId(id), uid, active, currencyPair, price, profit)

  object MonitorView:
    def from(m: Monitor): MonitorView = MonitorView(m.id.value, m.active, m.currencyPair, m.price, m.profit)

  private val basePath = "monitors"
  private val monitorIdPath = basePath / path[String]
    .validate(Controller.validId)
    .map((s: String) => MonitorId(s))(_.value)
    .name("monitor-id")
  private val compoundPath = basePath / "compound"

  val setupNewMonitorEndpoint = Controller.securedEndpoint.post
    .in(basePath)
    .in(jsonBody[CreateMonitorRequest])
    .out(statusCode(StatusCode.Created).and(jsonBody[CreateMonitorResponse]))
    .description("Setup new monitor")

  val deleteMonitorEndpoint = Controller.securedEndpoint.delete
    .in(monitorIdPath)
    .out(statusCode(StatusCode.NoContent))
    .description("Delete existing monitor")

  val pauseMonitorEndpoint = Controller.securedEndpoint.put
    .in(monitorIdPath / "pause")
    .out(statusCode(StatusCode.NoContent))
    .description("Pause existing monitor by updating its active status")

  val resumeMonitorEndpoint = Controller.securedEndpoint.put
    .in(monitorIdPath / "resume")
    .out(statusCode(StatusCode.NoContent))
    .description("Resume existing monitor by updating its active status")

  val queryMonitorEndpoint = Controller.securedEndpoint.post
    .in(monitorIdPath / "query")
    .out(statusCode(StatusCode.NoContent))
    .description("Manually query a monitor")

  val getAllMonitorsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .out(jsonBody[List[MonitorView]])
    .description("Retrieve all existing monitors")

  val getMonitorByIdEndpoint = Controller.securedEndpoint.get
    .in(monitorIdPath)
    .out(jsonBody[MonitorView])
    .description("Find monitor by id")

  val updateMonitorEndpoint = Controller.securedEndpoint.put
    .in(monitorIdPath)
    .in(jsonBody[MonitorView])
    .out(statusCode(StatusCode.NoContent))
    .description("Update monitor")

  val compoundUpdateMonitorEndpoint = Controller.securedEndpoint.put
    .in(compoundPath)
    .in(jsonBody[CompoundUpdateMonitorRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Update multiple monitors")

  def make[F[_]: Async](monitorService: MonitorService[F]): F[Controller[F]] =
    Monad[F].pure(MonitorController[F](monitorService))
}
