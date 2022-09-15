package currexx.clients.broker

import currexx.clients.broker
import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object Broker extends EnumType[Broker](() => Broker.values, _.print)
enum Broker:
  case Vindaloo, Xtb

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Vindaloo(externalId: String)                         extends BrokerParameters(Broker.Vindaloo)
  case Xtb(userId: String, password: String, demo: Boolean) extends BrokerParameters(Broker.Xtb)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Vindaloo.print -> JsonTaggedAdt.tagged[BrokerParameters.Vindaloo],
      Broker.Xtb.print      -> JsonTaggedAdt.tagged[BrokerParameters.Xtb]
    ),
    strict = true,
    typeFieldName = "broker"
  )
