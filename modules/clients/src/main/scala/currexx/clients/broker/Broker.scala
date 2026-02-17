package currexx.clients.broker

import org.latestbit.circe.adt.codec.*

enum Broker:
  case Oanda

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Oanda(apiKey: String, demo: Boolean, accountId: String) extends BrokerParameters(Broker.Oanda)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Oanda.toString.toLowerCase -> JsonTaggedAdt.tagged[BrokerParameters.Oanda]
    ),
    strict = true,
    typeFieldName = "broker"
  )
