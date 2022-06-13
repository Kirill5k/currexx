package currexx.core.monitor

import cats.Monad
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
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
        service
          .create(req.toDomain(session.userId))
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

  // TODO: add tests
  private def queryMonitor(using authenticator: Authenticator[F]) =
    queryMonitorEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .query(session.userId, mid)
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
          service
            .update(mon.toDomain(session.userId))
            .voidResponse
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

  final case class CreateMonitorRequest(
      currencyPair: CurrencyPair,
      interval: Interval,
      schedule: Schedule
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): CreateMonitor = CreateMonitor(uid, currencyPair, interval, schedule)

  final case class CreateMonitorResponse(id: MonitorId) derives Codec.AsObject

  final case class MonitorView(
      id: String,
      active: Boolean,
      currencyPair: CurrencyPair,
      interval: Interval,
      schedule: Schedule,
      lastQueriedAt: Option[Instant]
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): Monitor = Monitor(MonitorId(id), uid, active, currencyPair, interval, schedule, lastQueriedAt)

  object MonitorView:
    def from(m: Monitor): MonitorView = MonitorView(m.id.value, m.active, m.currencyPair, m.interval, m.schedule, m.lastQueriedAt)

  private val basePath = "monitors"
  private val monitorIdPath = basePath / path[String]
    .validate(Controller.validId)
    .map((s: String) => MonitorId(s))(_.value)
    .name("monitor-id")

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

  def make[F[_]: Async](monitorService: MonitorService[F]): F[Controller[F]] =
    Monad[F].pure(MonitorController[F](monitorService))
}
