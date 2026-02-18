package currexx.clients.broker

import currexx.domain.types.EnumType
import org.latestbit.circe.adt.codec.*

object Broker extends EnumType[Broker](() => Broker.values)
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
