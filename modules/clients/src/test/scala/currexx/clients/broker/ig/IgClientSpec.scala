package currexx.clients.broker.ig

import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.Currency.GBP
import kirill5k.common.sttp.test.SttpWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class IgClientSpec extends SttpWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config: IgConfig                  = IgConfig("ig.com")
  val brokerConfig: BrokerParameters.Ig = BrokerParameters.Ig("key", "user", "password", false, GBP)

  "IgClient" should {}
}
