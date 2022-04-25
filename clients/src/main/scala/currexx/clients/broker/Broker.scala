package currexx.clients.broker

import currexx.clients.broker
import io.circe.{Codec, Decoder, Encoder, Json}
import io.circe.syntax.*

enum Broker(val kind: String):
  case Vindaloo extends Broker("vindaloo")

object Broker:
  inline given Decoder[Broker] = Decoder[String].emap(b => Broker.values.find(_.kind == b).toRight(s"Unrecognized broker $b"))
  inline given Encoder[Broker] = Encoder[String].contramap(_.kind)

sealed trait BrokerParameters(val broker: Broker)

object BrokerParameters {
  final case class Vindaloo(externalId: String) extends BrokerParameters(Broker.Vindaloo) derives Codec.AsObject

  private val discriminatorField: String                    = "broker"
  private def discriminatorJson(bp: BrokerParameters): Json = Map(discriminatorField -> bp.broker).asJson

  inline given Decoder[BrokerParameters] = Decoder.instance { ip =>
    ip.downField(discriminatorField).as[Broker].flatMap { case Broker.Vindaloo =>
      ip.as[BrokerParameters.Vindaloo]
    }
  }
  inline given Encoder[BrokerParameters] = Encoder.instance { case vindaloo: broker.BrokerParameters.Vindaloo =>
    vindaloo.asJson.deepMerge(discriminatorJson(vindaloo))
  }
}
