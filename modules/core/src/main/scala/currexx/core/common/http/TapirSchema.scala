package currexx.core.common.http

import currexx.clients.broker.BrokerParameters
import currexx.core.market.MarketController.MarketStateView
import currexx.core.monitor.MonitorController.{CreateMonitorRequest, MonitorView}
import currexx.domain.market.{Currency, TradeOrder}
import currexx.domain.signal.{Condition, Direction, Indicator}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeStrategy
import currexx.domain.monitor.Schedule
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.{Schema, Validator}

import scala.concurrent.duration.FiniteDuration

transparent trait TapirSchema extends SchemaDerivation {
  given Schema[UserId] = Schema.string.description("User ID")
  given Schema[MonitorId] = Schema.string.description("Monitor ID")
  given Schema[IdString] = Schema.string.description("Generic ID")
  given Schema[NonEmptyString] = Schema.string.description("Non-empty string")
  given Schema[EmailString] = Schema.string.description("Email address").format("email")
  given Schema[Currency] = Schema.string.description("Currency code (e.g., EUR, USD)").pattern("^[A-Z]{3}$")

  given Schema[FiniteDuration] = Schema.string.description("Duration in ISO-8601 format, e.g., 'PT15M' for 15 minutes")

  given smDirection: Schema[Direction] = Schema.string.validate(Validator.enumeration(Direction.values.toList, d => Some(d.toString)))
  given smPosition: Schema[TradeOrder.Position] = Schema.string.validate(Validator.enumeration(TradeOrder.Position.values.toList, p => Some(p.toString)))

  // Schemas for complex types like MonitorView, CreateMonitorRequest, Indicator, Condition, etc.,
  // are now automatically derived by SchemaDerivation, removing the need for incorrect placeholders.
}
