package currexx.clients.broker

import currexx.domain.types.EnumType
import io.circe.derivation.{Configuration, ConfiguredCodec}

object Broker extends EnumType[Broker](() => Broker.values)
enum Broker:
  case Oanda

enum BrokerParameters(val broker: Broker) derives ConfiguredCodec:
  case Oanda(apiKey: String, demo: Boolean, accountId: String) extends BrokerParameters(Broker.Oanda)

object BrokerParameters:
  given Configuration = Configuration.default
    .withDiscriminator("broker")
    .withTransformConstructorNames(_.toLowerCase)
