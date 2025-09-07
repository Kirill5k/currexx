package currexx.clients.broker

import currexx.domain.market.Currency
import org.latestbit.circe.adt.codec.*

enum Broker:
  case Xtb, Ig

enum BrokerParameters(val broker: Broker) derives JsonTaggedAdt.EncoderWithConfig, JsonTaggedAdt.DecoderWithConfig:
  case Xtb(userId: String, password: String, demo: Boolean)                                      extends BrokerParameters(Broker.Xtb)
  case Ig(apiKey: String, username: String, password: String, demo: Boolean, currency: Currency) extends BrokerParameters(Broker.Ig)

object BrokerParameters:
  given JsonTaggedAdt.Config[BrokerParameters] = JsonTaggedAdt.Config.Values[BrokerParameters](
    mappings = Map(
      Broker.Xtb.toString.toLowerCase -> JsonTaggedAdt.tagged[BrokerParameters.Xtb],
      Broker.Ig.toString.toLowerCase  -> JsonTaggedAdt.tagged[BrokerParameters.Ig]
    ),
    strict = true,
    typeFieldName = "broker"
  )
