package currexx.clients.broker

import org.latestbit.circe.adt.codec.*

enum Broker:
  case Xtb, Oanda

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Xtb(userId: String, password: String, demo: Boolean)    extends BrokerParameters(Broker.Xtb)
  case Oanda(apiKey: String, demo: Boolean, accountId: String) extends BrokerParameters(Broker.Oanda)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Xtb.toString.toLowerCase   -> JsonTaggedAdt.tagged[BrokerParameters.Xtb],
      Broker.Oanda.toString.toLowerCase -> JsonTaggedAdt.tagged[BrokerParameters.Oanda]
    ),
    strict = true,
    typeFieldName = "broker"
  )
