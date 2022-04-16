package currexx.core.monitor

import cats.Monad
import cats.effect.Async
import currexx.core.auth.Authenticator
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import currexx.domain.market.{CurrencyPair, Interval}
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

  private def getMonitorById(using authenticator: Authenticator[F]) =
    getMonitorByIdEndpoint.withAuthenticatedSession
      .serverLogic { session => mid =>
        service
          .get(session.userId, mid)
          .mapResponse(MonitorView.from)
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
        getAllMonitors
      )
    )
}

object MonitorController extends TapirSchema with TapirJson {

  final case class CreateMonitorRequest(
      currencyPair: CurrencyPair,
      interval: Interval,
      period: FiniteDuration
  ) derives Codec.AsObject:
    def toDomain(uid: UserId): CreateMonitor = CreateMonitor(uid, currencyPair, interval, period)

  final case class CreateMonitorResponse(id: MonitorId) derives Codec.AsObject

  final case class MonitorView(
      id: String,
      active: Boolean,
      currencyPair: CurrencyPair,
      interval: Interval,
      period: FiniteDuration,
      lastQueriedAt: Option[Instant]
  ) derives Codec.AsObject

  object MonitorView:
    def from(m: Monitor): MonitorView = MonitorView(m.id.value, m.active, m.currencyPair, m.interval, m.period, m.lastQueriedAt)

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

  val getAllMonitorsEndpoint = Controller.securedEndpoint.get
    .in(basePath)
    .out(jsonBody[List[MonitorView]])
    .description("Retrieve all existing monitors")

  val getMonitorByIdEndpoint = Controller.securedEndpoint.get
    .in(monitorIdPath)
    .out(jsonBody[MonitorView])
    .description("Find monitor by id")

  def make[F[_]: Async](monitorService: MonitorService[F]): F[Controller[F]] =
    Monad[F].pure(MonitorController[F](monitorService))
}
