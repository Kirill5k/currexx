package currexx.core.monitor

import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import currexx.domain.market.{CurrencyPair, Interval}
import io.circe.Codec
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

object MonitorController extends TapirSchema with TapirJson {

  final case class CreateMonitorRequest(
      currencyPair: CurrencyPair,
      interval: Interval,
      period: FiniteDuration
  ) derives Codec.AsObject

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
  
  
}
