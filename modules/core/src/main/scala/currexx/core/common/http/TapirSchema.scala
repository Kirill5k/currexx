package currexx.core.common.http

import cats.data.{NonEmptyList, NonEmptySet}
import currexx.clients.broker.BrokerParameters
import currexx.core.monitor.MonitorController.{CreateMonitorRequest, MonitorView}
import currexx.domain.market.{Currency, Interval, TradeOrder}
import currexx.domain.signal.{Condition, Indicator}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.core.trade.TradeStrategy
import com.cronutils.model.Cron as JCron
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.{Schema, Validator}

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

transparent trait TapirSchema extends SchemaDerivation {
  given [T: {Schema, ClassTag}]: Schema[NonEmptyList[T]] = Schema
    .schemaForArray[T]
    .map(items => NonEmptyList.fromList(items.toList))(_.toList.toArray[T])

  given [T: {Schema, Ordering}]: Schema[NonEmptySet[T]] = Schema
    .schemaForSet[T, Set]
    .map(items => NonEmptySet.fromSet(SortedSet.from(items)))(_.toSortedSet.toSet)

  given Schema[JCron]          = Schema.string.description("Cron expression")
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

  // THESE SCHEMAS WILL NOT BE DERIVED AUTOMATICALLY:
  given Schema[Indicator]            = Schema.string
  given Schema[Condition]            = Schema.string
  given Schema[MonitorView]          = Schema.string
  given Schema[CreateMonitorRequest] = Schema.string
  given Schema[Interval]             = Schema.string
  given Schema[TradeOrder]           = Schema.string
  given Schema[TradeStrategy]        = Schema.string
  given Schema[BrokerParameters]     = Schema.string
}
