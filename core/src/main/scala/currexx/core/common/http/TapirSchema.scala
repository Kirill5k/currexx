package currexx.core.common.http

import cats.syntax.option.*
import currexx.domain.market.{Condition, Currency, Indicator, Interval, TradeOrder, Trend}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.domain.monitor.Schedule
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.{FieldName, Schema}
import sttp.tapir.Schema.SName
import sttp.tapir.SchemaType.{SProduct, SProductField}

import scala.concurrent.duration.FiniteDuration

transparent trait TapirSchema extends SchemaDerivation {
  // TODO: fix schemas
  inline given Schema[UserId]              = Schema.string
  inline given Schema[MonitorId]           = Schema.string
  inline given Schema[IdString]            = Schema.string
  inline given Schema[NonEmptyString]      = Schema.string
  inline given Schema[EmailString]         = Schema.string
  inline given Schema[Currency]            = Schema.string
  inline given Schema[FiniteDuration]      = Schema.string
  inline given Schema[Trend]               = Schema.string
  inline given Schema[Schedule]            = Schema.string
  inline given Schema[TradeOrder]          = Schema.string
  inline given Schema[TradeOrder.Position] = Schema.string
  inline given Schema[Indicator]           = Schema.string
  inline given Schema[Condition]           = Schema.string
}
