package currexx.core.common.http

import currexx.clients.broker.BrokerParameters
import currexx.core.market.MarketController.MarketStateView
import currexx.core.monitor.MonitorController.{CreateMonitorRequest, MonitorView}
import currexx.domain.market.Currency
import currexx.domain.signal.{Condition, Direction, Indicator}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeStrategy
import currexx.domain.market.TradeOrder
import currexx.domain.monitor.Schedule
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.{Schema, Validator}

import scala.concurrent.duration.FiniteDuration

transparent trait TapirSchema extends SchemaDerivation {
  given Schema[FiniteDuration] = Schema.string

  given Schema[IdString]       = Schema.string.description("Generic ID")
  given Schema[NonEmptyString] = Schema.string.description("Non-empty string")

  given Schema[UserId]      = Schema.string.description("User ID")
  given Schema[MonitorId]   = Schema.string.description("Monitor ID")
  given Schema[EmailString] = Schema.string.description("Email address").format("email")
  given Schema[Currency]    = Schema.string
    .validate(Validator.pattern("^[A-Z]{3}$"))
    .map[Currency](s => Currency.from(s).toOption)(_.code)
    .description("3-letter currency code (e.g., EUR, USD)")

  given Schema[Direction]           = Schema.string.validate(enumValidator(Direction.values))
  given Schema[TradeOrder.Position] = Schema.string.validate(enumValidator(TradeOrder.Position.values))

  // Schemas for complex types like MonitorView, CreateMonitorRequest, Indicator, Condition, etc.,
  // are now automatically derived by SchemaDerivation, removing the need for incorrect placeholders.
  given Schema[Indicator]            = Schema.string
  given Schema[Condition]            = Schema.string
  given Schema[MonitorView]          = Schema.string
  given Schema[CreateMonitorRequest] = Schema.string
  given Schema[Schedule]             = Schema.string
  given Schema[TradeOrder]           = Schema.string
  given Schema[TradeStrategy]        = Schema.string
  given Schema[BrokerParameters]     = Schema.string
  given Schema[MarketStateView]      = Schema.string

  private def enumValidator[E](values: Array[E]): Validator[E] =
    Validator.enumeration[E](values.toList, e => Some(e.toString))
}
