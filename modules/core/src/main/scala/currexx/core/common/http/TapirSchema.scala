package currexx.core.common.http

import currexx.clients.broker.BrokerParameters
import currexx.core.market.MarketController.MarketStateView
import currexx.core.monitor.MonitorController.{CreateMonitorRequest, MonitorView}
import currexx.domain.market.{Condition, Currency, Direction, Indicator, TradeOrder}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.core.settings.TriggerFrequency
import currexx.core.trade.TradeStrategy
import currexx.domain.monitor.Schedule
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.Schema

import scala.concurrent.duration.FiniteDuration

transparent trait TapirSchema extends SchemaDerivation {
  // TODO: fix schemas
  given Schema[UserId]               = Schema.string
  given Schema[MonitorId]            = Schema.string
  given Schema[IdString]             = Schema.string
  given Schema[NonEmptyString]       = Schema.string
  given Schema[EmailString]          = Schema.string
  given Schema[Currency]             = Schema.string
  given Schema[MonitorView]          = Schema.string
  given Schema[CreateMonitorRequest] = Schema.string
  given Schema[FiniteDuration]       = Schema.string
  given Schema[Direction]            = Schema.string
  given Schema[Schedule]             = Schema.string
  given Schema[TradeOrder]           = Schema.string
  given Schema[TradeOrder.Position]  = Schema.string
  given Schema[Indicator]            = Schema.string
  given Schema[Condition]            = Schema.string
  given Schema[TriggerFrequency]     = Schema.string
  given Schema[TradeStrategy]        = Schema.string
  given Schema[BrokerParameters]     = Schema.string
  given Schema[MarketStateView]      = Schema.string
}
