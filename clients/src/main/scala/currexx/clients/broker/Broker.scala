package currexx.clients.broker

import currexx.clients.broker
import org.latestbit.circe.adt.codec.*

enum Broker(val kind: String) derives JsonTaggedAdt.PureEncoderWithConfig, JsonTaggedAdt.PureDecoderWithConfig:
  case Vindaloo extends Broker("vindaloo")

object Broker:
  given JsonTaggedAdt.PureConfig[Broker] = JsonTaggedAdt.PureConfig.Values[Broker](
    mappings = Map(
      Broker.Vindaloo.kind -> JsonTaggedAdt.tagged[Broker.Vindaloo.type]
    )
  )

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Vindaloo(externalId: String) extends BrokerParameters(Broker.Vindaloo)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Vindaloo.kind -> JsonTaggedAdt.tagged[BrokerParameters.Vindaloo]
    ),
    strict = true,
    typeFieldName = "broker"
  )
