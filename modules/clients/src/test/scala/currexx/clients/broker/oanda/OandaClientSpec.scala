package currexx.clients.broker.oanda

import cats.effect.IO
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class OandaClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]
  
  "OandaClient" should {
    // Tests would go here
  }
}
