package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.Currency.{EUR, GBP, USD}
import currexx.domain.market.{CurrencyPair, TradeOrder}
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

class OandaClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config                         = OandaConfig("https://api-fxpractice.oanda.com", "https://api-fxtrade.oanda.com")
  val params: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = true, None)
  val eurUsdPair                     = CurrencyPair(EUR, USD)
  val gbpUsdPair                     = CurrencyPair(GBP, USD)

  "An OandaClient" should {
    "submit enter buy order successfully" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isPost && r.hasPath("/v3/accounts/123-456-789/orders") =>
            ResponseStub.adjust("", StatusCode.Created)
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield ()

      result.asserting(_ mustBe ())
    }

    "submit enter sell order successfully" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isPost && r.hasPath("/v3/accounts/123-456-789/orders") =>
            ResponseStub.adjust("", StatusCode.Created)
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Sell, eurUsdPair, BigDecimal(1), BigDecimal("0.5")))
      yield ()

      result.asserting(_ mustBe ())
    }

    "submit exit order successfully when position exists" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
            ResponseStub.adjust(readJson("oanda/position-success-response.json"))
          case r if r.isPut && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD/close") =>
            ResponseStub.adjust("""{"lastTransactionID":"1"}""")
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Exit(eurUsdPair, BigDecimal(1)))
      yield ()

      result.asserting(_ mustBe ())
    }

    "skip closing position when no position exists" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
            ResponseStub.adjust(readJson("oanda/closed-position-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Exit(eurUsdPair, BigDecimal(1)))
      yield ()

      result.asserting(_ mustBe ())
    }

    "retrieve current orders successfully" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions") =>
            ResponseStub.adjust(readJson("oanda/positions-success-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair, gbpUsdPair))
      yield orders

      result.asserting { orders =>
        orders must have size 2

        val eurUsdOrder = orders.find(_.currencyPair == eurUsdPair).get
        eurUsdOrder.position mustBe TradeOrder.Position.Buy
        eurUsdOrder.openPrice mustBe BigDecimal("1.1050")
        eurUsdOrder.volume mustBe BigDecimal("1.0")
        eurUsdOrder.profit mustBe BigDecimal("50.00")

        val gbpUsdOrder = orders.find(_.currencyPair == gbpUsdPair).get
        gbpUsdOrder.position mustBe TradeOrder.Position.Sell
        gbpUsdOrder.openPrice mustBe BigDecimal("1.2500")
        gbpUsdOrder.volume mustBe BigDecimal("0.5")
        gbpUsdOrder.profit mustBe BigDecimal("-25.00")
      }
    }

    "filter positions by requested currency pairs" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions") =>
            ResponseStub.adjust(readJson("oanda/positions-success-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair))
      yield orders

      result.asserting { orders =>
        orders must have size 1
        orders.head.currencyPair mustBe eurUsdPair
      }
    }

    "handle API errors when submitting orders" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("api-fxpractice.oanda.com/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isPost && r.isGoingTo("api-fxpractice.oanda.com/v3/accounts/123-456-789/orders") =>
            ResponseStub.adjust("Order submission failed", StatusCode.BadRequest)
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield ()

      result.assertThrows(AppError.ClientFailure("oanda", "Open position returned 400"))
    }

    "handle JSON parsing errors" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("api-fxpractice.oanda.com/v3/accounts") =>
            ResponseStub.adjust("invalid json")
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair))
      yield orders

      result.assertThrows(
        AppError.JsonParsingFailure("invalid json", "oanda client returned ParsingFailure: expected json value got 'invali...' (line 1, column 1)")
      )
    }

    "use live API URL when demo is false" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasHost("api-fxtrade.oanda.com") && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isPost && r.hasHost("api-fxtrade.oanda.com") && r.hasPath("/v3/accounts/123-456-789/orders") =>
            ResponseStub.adjust("", StatusCode.Created)
          case _ => throw new RuntimeException("Unexpected request")
        }

      val liveParams: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = false, None)
      val result                             = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(liveParams, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield ()

      result.asserting(_ mustBe ())
    }

    "handle empty accounts list" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("api-fxpractice.oanda.com/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/empty-accounts-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaClient.make[IO](config, testingBackend)
        _      <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield ()

      result.assertThrows(AppError.ClientFailure("oanda", s"Get accounts returned empty accounts list"))
    }
  }
}
