package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.Currency.{EUR, GBP, USD}
import currexx.domain.market.{CurrencyPair, OrderPlacementStatus, TradeOrder}
import io.circe.Json
import io.circe.parser.parse
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

class OandaBrokerClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config                         = OandaBrokerConfig("https://api-fxpractice.oanda.com", "https://api-fxtrade.oanda.com")
  val params: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = true, "123-456-789")
  val eurUsdPair                     = CurrencyPair(EUR, USD)
  val gbpUsdPair                     = CurrencyPair(GBP, USD)

  "An OandaBrokerClient" should {
    "submit enter buy order successfully without response" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isPost && r.hasPath("/v3/accounts/123-456-789/orders") =>
            ResponseStub.adjust("", StatusCode.Created)
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        status <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield status

      result.asserting(_ mustBe OrderPlacementStatus.Pending)
    }

    List(
      ("filled", "orderFillTransaction", """{"type":"ORDER_FILL","units":"-50000","pl":"0.0"}""", OrderPlacementStatus.Success),
      (
        "cancelled",
        "orderCancelTransaction",
        """{"type":"ORDER_CANCEL","reason":"MARKET_HALTED"}""",
        OrderPlacementStatus.Cancelled("MARKET_HALTED")
      )
    ).foreach { case (description, outcomeField, outcome, expectedStatus) =>
      s"report a $description entry when create and outcome transactions omit request IDs" in {
        val transaction = parse("""{
          "id":"201", "time":"2026-09-28T09:00:00Z", "userID":123,
          "accountID":"123-456-789", "batchID":"201"
        }""").toOption.get
        val response = Json
          .obj(
            "orderCreateTransaction" -> transaction,
            outcomeField             -> transaction.deepMerge(parse(outcome).toOption.get).mapObject(_.add("id", Json.fromString("202"))),
            "relatedTransactionIDs"  -> Json.arr(Json.fromString("201"), Json.fromString("202")),
            "lastTransactionID"      -> Json.fromString("202")
          )
          .noSpaces
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/v3/accounts") =>
              ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
            case r if r.isPost && r.hasPath("/v3/accounts/123-456-789/orders") =>
              ResponseStub.adjust(response, StatusCode.Created)
            case _ => throw new RuntimeException("Unexpected request")
          }

        val result = for
          client <- OandaBrokerClient.make[IO](config, testingBackend)
          status <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Sell, eurUsdPair, BigDecimal(1), BigDecimal("0.5")))
        yield status

        result.asserting(_ mustBe expectedStatus)
      }
    }

    List(
      ("long", 100000, 0, List("longOrderFillTransaction")),
      ("short", 0, -100000, List("shortOrderFillTransaction")),
      ("long and short", 100000, -100000, List("longOrderFillTransaction", "shortOrderFillTransaction"))
    ).foreach { case (sides, longUnits, shortUnits, transactions) =>
      s"submit an exit successfully when every requested $sides side is filled" in
        submitExit(longUnits, shortUnits, closeResponse(transactions*))
          .asserting(_ mustBe OrderPlacementStatus.Success)
    }

    List(
      ("long", 100000, 0, """"longOrderFillTransaction":{}"""),
      ("short", 0, -100000, """"shortOrderFillTransaction":{}"""),
      ("both sides", 100000, -100000, """"longOrderFillTransaction":{},"shortOrderFillTransaction":{}"""),
      ("long with only an ID in the fill", 100000, 0, """"longOrderFillTransaction":{"id":"1"}""")
    ).foreach { case (description, longUnits, shortUnits, transactions) =>
      s"accept a $description close with minimal fill objects" in {
        val response = s"""{"lastTransactionID":"1",$transactions}"""

        submitExit(longUnits, shortUnits, response).asserting(_ mustBe OrderPlacementStatus.Success)
      }
    }

    List(
      (
        "both cancelled sides",
        """"longOrderCancelTransaction":{"reason":"LONG_CANCELLED"},"shortOrderCancelTransaction":{"reason":"SHORT_CANCELLED"}""",
        "LONG_CANCELLED; SHORT_CANCELLED"
      ),
      (
        "a cancelled long side and filled short side",
        """"longOrderCancelTransaction":{"reason":"LONG_CANCELLED"},"shortOrderFillTransaction":{}""",
        "LONG_CANCELLED"
      ),
      (
        "a filled long side and cancelled short side",
        """"longOrderFillTransaction":{},"shortOrderCancelTransaction":{"reason":"SHORT_CANCELLED"}""",
        "SHORT_CANCELLED"
      )
    ).foreach { case (description, transactions, reason) =>
      s"accept minimal transaction objects for $description" in {
        val response = s"""{"lastTransactionID":"1",$transactions}"""

        submitExit(100000, -100000, response).asserting(_ mustBe OrderPlacementStatus.Cancelled(reason))
      }
    }

    List(
      ("fill", "longOrderFillTransaction", Json.obj(), OrderPlacementStatus.Success),
      (
        "cancellation",
        "longOrderCancelTransaction",
        Json.obj("reason" -> Json.fromString("MARKET_HALTED")),
        OrderPlacementStatus.Cancelled("MARKET_HALTED")
      )
    ).foreach { case (description, field, requiredFields, expectedStatus) =>
      s"ignore unused $description metadata with unexpected shapes" in {
        val metadata = parse("""{
          "id": 1, "time": [], "userID": "user", "accountID": false,
          "batchID": {}, "requestID": 2, "type": {}, "units": {}, "pl": "unknown",
          "tradeOpened": {}, "tradesClosed": 3, "tradeReduced": "changed"
        }""").toOption.get
        val response = Json
          .obj(
            "lastTransactionID" -> Json.fromString("1"),
            field               -> metadata.deepMerge(requiredFields)
          )
          .noSpaces

        submitExit(100000, 0, response).asserting(_ mustBe expectedStatus)
      }
    }

    List(
      ("long", 100000, 0, List("longOrderCancelTransaction"), "FOK_ORDER_IMMEDIATE_PARTIAL_FILL"),
      ("short", 0, -100000, List("shortOrderCancelTransaction"), "MARKET_HALTED"),
      (
        "both sides",
        100000,
        -100000,
        List("longOrderCancelTransaction", "shortOrderCancelTransaction"),
        "FOK_ORDER_IMMEDIATE_PARTIAL_FILL; MARKET_HALTED"
      ),
      (
        "long with a filled short side",
        100000,
        -100000,
        List("longOrderCancelTransaction", "shortOrderFillTransaction"),
        "FOK_ORDER_IMMEDIATE_PARTIAL_FILL"
      ),
      (
        "short with a filled long side",
        100000,
        -100000,
        List("longOrderFillTransaction", "shortOrderCancelTransaction"),
        "MARKET_HALTED"
      ),
      (
        "an unrequested short side with a filled long side",
        100000,
        0,
        List("longOrderFillTransaction", "shortOrderCancelTransaction"),
        "MARKET_HALTED"
      )
    ).foreach { case (sides, longUnits, shortUnits, transactions, reason) =>
      s"return cancelled when closing $sides is cancelled in an HTTP 200 response" in
        submitExit(longUnits, shortUnits, closeResponse(transactions*))
          .asserting(_ mustBe OrderPlacementStatus.Cancelled(reason))
    }

    List(
      ("long with only a transaction ID", 100000, 0, List.empty[String], "long"),
      ("short with only a transaction ID", 0, -100000, List.empty[String], "short"),
      ("both sides with only a transaction ID", 100000, -100000, List.empty[String], "long, short"),
      ("both sides with only a long fill", 100000, -100000, List("longOrderFillTransaction"), "short"),
      ("both sides with only a short fill", 100000, -100000, List("shortOrderFillTransaction"), "long"),
      ("long with only an unrequested short fill", 100000, 0, List("shortOrderFillTransaction"), "long"),
      ("short with only an unrequested long fill", 0, -100000, List("longOrderFillTransaction"), "short")
    ).foreach { case (description, longUnits, shortUnits, transactions, missingSides) =>
      s"reject an unconfirmed close of $description" in
        submitExit(longUnits, shortUnits, closeResponse(transactions*)).assertThrows(
          AppError.ClientFailure("oanda", s"close-position for EUR_USD missing fill confirmation for $missingSides")
        )
    }

    "reject a null fill transaction as missing confirmation" in
      submitExit(100000, 0, """{"lastTransactionID":"1","longOrderFillTransaction":null}""")
        .assertThrows(AppError.ClientFailure("oanda", "close-position for EUR_USD missing fill confirmation for long"))

    List(
      ("invalid JSON", "invalid json"),
      ("an invalid fill transaction", """{"lastTransactionID":"1","longOrderFillTransaction":"invalid"}"""),
      ("an array fill transaction", """{"lastTransactionID":"1","longOrderFillTransaction":[]}"""),
      ("a numeric fill transaction", """{"lastTransactionID":"1","longOrderFillTransaction":1}"""),
      ("an invalid short fill transaction", """{"lastTransactionID":"1","shortOrderFillTransaction":false}"""),
      ("an incomplete cancellation transaction", """{"lastTransactionID":"1","longOrderCancelTransaction":{"id":"1"}}"""),
      ("a nonstring cancellation reason", """{"lastTransactionID":"1","longOrderCancelTransaction":{"reason":1}}"""),
      ("an invalid cancellation transaction", """{"lastTransactionID":"1","shortOrderCancelTransaction":[]}""")
    ).foreach { case (description, response) =>
      s"preserve JSON parsing failures for close responses with $description" in
        submitExit(100000, 0, response).attempt.asserting {
          case Left(AppError.JsonParsingFailure(original, _)) => original mustBe response
          case result                                         => fail(s"Expected a JSON parsing failure, received $result")
        }
    }

    "propagate HTTP errors when closing a position" in
      submitExit(100000, 0, "Close rejected", StatusCode.BadRequest)
        .assertThrows(AppError.ClientFailure("oanda", "close-position returned 400: Close rejected"))

    "return no-position status when no position exists" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.hasPath("/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
          case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
            ResponseStub.adjust(readJson("oanda/closed-position-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        status <- client.submit(params, TradeOrder.Exit(eurUsdPair, BigDecimal(1)))
      yield status

      result.asserting(_ mustBe OrderPlacementStatus.NoPosition)
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
        client <- OandaBrokerClient.make[IO](config, testingBackend)
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
        client <- OandaBrokerClient.make[IO](config, testingBackend)
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
        client <- OandaBrokerClient.make[IO](config, testingBackend)
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
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair))
      yield orders

      result.assertThrows(
        AppError.JsonParsingFailure(
          "invalid json",
          "oanda client returned ParsingFailure: expected json value got 'invali...' (line 1, column 1)"
        )
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

      val liveParams: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = false, "123-456-789")
      val result                             = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        status <- client.submit(liveParams, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield status

      result.asserting(_ mustBe OrderPlacementStatus.Pending)
    }

    "handle invalid account id" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("api-fxpractice.oanda.com/v3/accounts") =>
            ResponseStub.adjust(readJson("oanda/empty-accounts-response.json"))
          case _ => throw new RuntimeException("Unexpected request")
        }

      val result = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        _      <- client.submit(
          BrokerParameters.Oanda("test-api-key", demo = true, "123"),
          TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0"))
        )
      yield ()

      result.assertThrows(AppError.ClientFailure("oanda", s"Account id 123 does not exist"))
    }

    "retry on server errors and eventually succeed" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatches(_.hasPath("/v3/accounts"))
        .thenRespondCyclic(
          ResponseStub.adjust("Server error", StatusCode.InternalServerError),
          ResponseStub.adjust("Server error", StatusCode.InternalServerError),
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        )
        .whenRequestMatches(_.hasPath("/v3/accounts/123-456-789/positions"))
        .thenRespond(ResponseStub.adjust(readJson("oanda/positions-success-response.json")))

      val result = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair, gbpUsdPair))
      yield orders

      result.asserting { orders =>
        orders must have size 2
      }
    }

    "retry on server errors and fail after max retries" in {
      val testingBackend = fs2BackendStub
        .whenRequestMatches(_.hasPath("/v3/accounts"))
        .thenRespondCyclic(ResponseStub.adjust("Server error", StatusCode.ServiceUnavailable))

      val result = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        orders <- client.getCurrentOrders(params, NonEmptyList.of(eurUsdPair))
      yield orders

      result.assertThrows(AppError.ClientFailure("oanda", "get-account returned 503: Server error"))
    }
  }

  private def closeResponse(transactions: String*): String = {
    val fills         = parse(readJson("oanda/close-position-success-response.json")).toOption.get
    val cancellations = parse(readJson("oanda/close-position-cancelled-response.json")).toOption.get
    val fields        = transactions.map { field =>
      val response = if field.endsWith("CancelTransaction") then cancellations else fills
      field -> response.hcursor.downField(field).focus.get
    }
    Json.obj((fields :+ ("lastTransactionID" -> Json.fromString("204")))*).noSpaces
  }

  private def submitExit(
      longUnits: Int,
      shortUnits: Int,
      response: String,
      responseStatus: StatusCode = StatusCode.Ok
  ): IO[OrderPlacementStatus] = {
    val positionResponse = s"""{
      "position": {
        "instrument": "EUR_USD",
        "long": {"units": "$longUnits", "trueUnrealizedPL": "0.00", "unrealizedPL": "0.00"},
        "short": {"units": "$shortUnits", "trueUnrealizedPL": "0.00", "unrealizedPL": "0.00"}
      }
    }"""
    val testingBackend = fs2BackendStub
      .whenRequestMatchesPartial {
        case r if r.isGet && r.hasPath("/v3/accounts") =>
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
          ResponseStub.adjust(positionResponse)
        case r if r.isPut && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD/close") =>
          ResponseStub.adjust(response, responseStatus)
        case _ => throw new RuntimeException("Unexpected request")
      }

    for
      client <- OandaBrokerClient.make[IO](config, testingBackend)
      status <- client.submit(params, TradeOrder.Exit(eurUsdPair, BigDecimal(1)))
    yield status
  }
}
