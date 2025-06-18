package currexx.clients.broker

import org.latestbit.circe.adt.codec.*

enum Broker:
  case Xtb

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Xtb(userId: String, password: String, demo: Boolean) extends BrokerParameters(Broker.Xtb)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Xtb.toString.toLowerCase -> JsonTaggedAdt.tagged[BrokerParameters.Xtb]
    ),
    strict = true,
    typeFieldName = "broker"
  )
