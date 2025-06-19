package currexx.clients.broker.xtb

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.Currency.{CAD, EUR}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import kirill5k.common.sttp.test.SttpWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.BackendOptions
import sttp.client4.httpclient.fs2.HttpClientFs2Backend as Fs2Backend

import scala.concurrent.duration.*

class XtbClientSpec extends SttpWordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]
  
  val config = XtbConfig("wss://ws.xtb.com")
  val pair   = CurrencyPair(EUR, CAD)
  val price  = BigDecimal(1.341)

  val brokerConfig: BrokerParameters.Xtb = BrokerParameters.Xtb("foo", "bar", true)

  "A XtbClient" should {
    
    "send enter market request" ignore {
      val result = Fs2Backend
        .resource[IO](options = BackendOptions(3.minutes, None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            order = TradeOrder.Enter(TradeOrder.Position.Buy, pair, price, BigDecimal(0.1))
            res <- client.submit(brokerConfig, order)
          yield res
        }

      result.assertVoid
    }

    "get existing order" ignore {
      val result = Fs2Backend
        .resource[IO](options = BackendOptions(3.minutes, None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            res    <- client.getCurrentOrders(brokerConfig, NonEmptyList.of(pair))
          yield res
        }

      result.asserting { order =>
        order.map(_.currencyPair) mustBe Some(pair)
      }
    }

    "send exit market request" ignore {
      val result = Fs2Backend
        .resource[IO](options = BackendOptions(3.minutes, None))
        .use { backend =>
          for
            client <- XtbClient.make[IO](config, backend)
            _      <- IO.sleep(10.seconds)
            res    <- client.submit(brokerConfig, TradeOrder.Exit(pair, price))
          yield res
        }

      result.assertVoid
    }
  }
}
