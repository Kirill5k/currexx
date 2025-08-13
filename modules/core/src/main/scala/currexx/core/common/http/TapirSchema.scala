package currexx.core.common.http

import cats.data.{NonEmptyList, NonEmptySet}
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.{Currency, Interval, TradeOrder}
import currexx.domain.signal.{Condition, Indicator, ValueTransformation}
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.validations.{EmailString, IdString}
import currexx.domain.user.UserId
import currexx.core.monitor.MonitorId
import currexx.core.trade.Rule.Condition as RuleCondition
import com.cronutils.model.Cron as JCron
import currexx.domain.monitor.Schedule
import sttp.tapir.generic.Configuration
import sttp.tapir.generic.auto.SchemaDerivation
import sttp.tapir.{Schema, Validator}

import scala.collection.immutable.SortedSet
import scala.concurrent.duration.FiniteDuration
import scala.reflect.ClassTag

transparent trait TapirSchema extends SchemaDerivation {
  given Configuration = Configuration.default.withDiscriminator("kind")

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

  given Schema[Interval] = Schema.string.validate(Validator.enumeration(Interval.values.toList, i => Some(i.toString)))

  given Schema[TradeOrder]          = Schema.derived
  given Schema[BrokerParameters]    = Schema.derived
  given Schema[Schedule]            = Schema.derived
  given Schema[RuleCondition]       = Schema.derived
  given Schema[ValueTransformation] = Schema.derived
  given Schema[Condition]           = Schema.string
  given Schema[Indicator]           = Schema.string
}
