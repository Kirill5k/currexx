package currexx.clients.broker

import org.latestbit.circe.adt.codec.*

enum Broker:
  case Vindaloo, Xtb

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Vindaloo(externalId: String)                         extends BrokerParameters(Broker.Vindaloo)
  case Xtb(userId: String, password: String, demo: Boolean) extends BrokerParameters(Broker.Xtb)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Vindaloo.toString.toLowerCase -> JsonTaggedAdt.tagged[BrokerParameters.Vindaloo],
      Broker.Xtb.toString.toLowerCase      -> JsonTaggedAdt.tagged[BrokerParameters.Xtb]
    ),
    strict = true,
    typeFieldName = "broker"
  )
