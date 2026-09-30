package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.errors.AppError
import currexx.domain.market.Currency.{EUR, GBP, USD}
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderExecution, OrderPlacementResult, OrderRef, TradeOrder}
import io.circe.{Json, JsonObject}
import io.circe.parser.parse
import kirill5k.common.cats.Clock
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

import java.net.SocketTimeoutException
import java.time.Instant
import java.util.concurrent.atomic.AtomicReference

class OandaBrokerClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config                         = OandaBrokerConfig("https://api-fxpractice.oanda.com", "https://api-fxtrade.oanda.com")
  val params: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = true, "123-456-789")
  val eurUsdPair                     = CurrencyPair(EUR, USD)
  val gbpUsdPair                     = CurrencyPair(GBP, USD)

  val fillTime                = Instant.parse("2026-09-28T09:00:00Z")
  val longCloseExecution      = OrderExecution(BigDecimal("1.1055"), fillTime, BigDecimal(1), "201", "202", List("123"))
  val shortCloseExecution     = OrderExecution(BigDecimal("1.1057"), fillTime, BigDecimal(1), "203", "204", List("124"))
  val lookedUpEntryExecution  = OrderExecution(BigDecimal("1.10502"), fillTime.plusSeconds(1), BigDecimal(1), "42", "43", List("43"))
  val fillTransactionResponse = """{
    "transaction": {
      "id": "43", "time": "2026-09-28T09:00:01Z", "userID": 123, "accountID": "123-456-789", "batchID": "42",
      "type": "ORDER_FILL", "orderID": "42", "instrument": "EUR_USD", "units": "100000",
      "price": "1.1050", "fullVWAP": "1.10502", "pl": "0.0000",
      "tradeOpened": {"tradeID": "43", "units": "100000", "price": "1.10502"}
    },
    "lastTransactionID": "43"
  }"""

  "An OandaBrokerClient" should {
    List(
      (
        "filled",
        "orderFillTransaction",
        """{"type":"ORDER_FILL","orderID":"201","units":"-50000","price":"1.1050","pl":"0.0","tradeOpened":{"tradeID":"202"}}""",
        OrderPlacementResult.filled(OrderExecution(BigDecimal("1.1050"), fillTime, BigDecimal("0.5"), "201", "202", List("202")))
      ),
      (
        "cancelled",
        "orderCancelTransaction",
        """{"type":"ORDER_CANCEL","reason":"MARKET_HALTED"}""",
        OrderPlacementResult.Cancelled("MARKET_HALTED")
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

    "return a pending entry referencing both order IDs when the order is created without an outcome" in
      submitEnterWithUnknownOutcome(
        EntrySubmission.CreatedWith(
          """{
            "orderCreateTransaction": {"id":"201", "time":"2026-09-28T09:00:00Z", "userID":123, "accountID":"123-456-789", "batchID":"201"},
            "relatedTransactionIDs": ["201"],
            "lastTransactionID": "201"
          }"""
        ),
        "unused"
      ).asserting {
        case (Right(OrderPlacementResult.Pending(OrderRef(clientOrderId, Some("201")))), List(submission)) =>
          submission must include(clientOrderId)
        case result => fail(s"Expected a pending entry, received $result")
      }

    List(
      ("long", 100000, 0, List("longOrderFillTransaction"), longCloseExecution),
      ("short", 0, -100000, List("shortOrderFillTransaction"), shortCloseExecution)
    ).foreach { case (side, longUnits, shortUnits, transactions, expectedExecution) =>
      s"return the execution of a filled $side close" in
        submitExit(longUnits, shortUnits, closeResponse(transactions*))
          .asserting(_ mustBe OrderPlacementResult.filled(expectedExecution))
    }

    "return the executions of both sides when a close fills long and short" in
      submitExit(100000, -100000, closeResponse("longOrderFillTransaction", "shortOrderFillTransaction"))
        .asserting(_ mustBe OrderPlacementResult.Filled(NonEmptyList.of(longCloseExecution, shortCloseExecution)))

    "return a fill without execution details when one side of a two-sided close cannot be decoded" in {
      val longFill = parse(closeResponse("longOrderFillTransaction")).toOption.get.hcursor.downField("longOrderFillTransaction").focus.get
      val response = Json
        .obj(
          "lastTransactionID"         -> Json.fromString("204"),
          "longOrderFillTransaction"  -> longFill,
          "shortOrderFillTransaction" -> Json.obj("orderID" -> Json.fromString("203"))
        )
        .noSpaces

      submitExit(100000, -100000, response).asserting(_ mustBe OrderPlacementResult.FilledWithoutExecution(Some("201")))
    }

    List(
      ("an empty fill", """"longOrderFillTransaction":{}""", None),
      ("a fill with only an order ID", """"longOrderFillTransaction":{"orderID":"201"}""", Some("201"))
    ).foreach { case (description, transactions, orderId) =>
      s"return a fill without execution details for a close with $description" in {
        val response = s"""{"lastTransactionID":"1",$transactions}"""

        submitExit(100000, 0, response).asserting(_ mustBe OrderPlacementResult.FilledWithoutExecution(orderId))
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

        submitExit(100000, -100000, response).asserting(_ mustBe OrderPlacementResult.Cancelled(reason))
      }
    }

    List(
      (
        "fill",
        "longOrderFillTransaction",
        parse(readJson("oanda/close-position-success-response.json")).toOption.get.hcursor.downField("longOrderFillTransaction").focus.get,
        OrderPlacementResult.filled(longCloseExecution)
      ),
      (
        "cancellation",
        "longOrderCancelTransaction",
        Json.obj("reason" -> Json.fromString("MARKET_HALTED")),
        OrderPlacementResult.Cancelled("MARKET_HALTED")
      )
    ).foreach { case (description, field, requiredFields, expectedStatus) =>
      s"ignore unused $description metadata with unexpected shapes" in {
        val metadata = parse("""{
          "id": 1, "time": [], "userID": "user", "accountID": false,
          "batchID": {}, "requestID": 2, "type": {}, "units": {}, "pl": "unknown", "reason": 5
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
          .asserting(_ mustBe OrderPlacementResult.Cancelled(reason))
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

    List(
      ("missing units", (side: JsonObject) => side.remove("units")),
      ("malformed units", (side: JsonObject) => side.add("units", Json.fromString("invalid"))),
      ("missing unrealizedPL", (side: JsonObject) => side.remove("unrealizedPL")),
      ("malformed unrealizedPL", (side: JsonObject) => side.add("unrealizedPL", Json.fromString("invalid")))
    ).foreach { case (description, adjustSide) =>
      s"reject a position with $description without sending a close" in
        submitExit(100000, 0, closeResponse("longOrderFillTransaction"), adjustSide = adjustSide, expectClose = false).attempt.asserting {
          case Left(AppError.JsonParsingFailure(_, _)) => succeed
          case result                                  => fail(s"Expected a JSON parsing failure, received $result")
        }
    }

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

      result.asserting(_ mustBe OrderPlacementResult.NoPosition)
    }

    List(
      ("absent", Option.empty[Json]),
      ("a conflicting number", Some(Json.fromInt(999))),
      ("null", Some(Json.Null)),
      ("malformed", Some(Json.obj("unexpected" -> Json.True)))
    ).foreach { case (description, extension) =>
      val adjustSide: JsonObject => JsonObject =
        side => extension.fold(side.remove("trueUnrealizedPL"))(value => side.add("trueUnrealizedPL", value))

      s"submit an exit when trueUnrealizedPL is $description" in
        submitExit(100000, 0, closeResponse("longOrderFillTransaction"), adjustSide = adjustSide)
          .asserting(_ mustBe OrderPlacementResult.filled(longCloseExecution))

      s"retrieve current orders using documented profit when trueUnrealizedPL is $description" in {
        val response = parse(readJson("oanda/positions-success-response.json")).toOption.get.hcursor
          .downField("positions")
          .withFocus(_.mapArray(_.map(position => mapPositionSides(position, adjustSide))))
          .top
          .get
          .noSpaces
        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.hasPath("/v3/accounts") =>
              ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
            case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions") =>
              ResponseStub.adjust(response)
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
          eurUsdOrder.profit mustBe BigDecimal("52.00")

          val gbpUsdOrder = orders.find(_.currencyPair == gbpUsdPair).get
          gbpUsdOrder.position mustBe TradeOrder.Position.Sell
          gbpUsdOrder.openPrice mustBe BigDecimal("1.2500")
          gbpUsdOrder.volume mustBe BigDecimal("0.5")
          gbpUsdOrder.profit mustBe BigDecimal("-27.00")
        }
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

    "fail to retrieve current orders when a requested position is open on both sides" in
      currentOrdersWithHedgedEurUsd(NonEmptyList.of(eurUsdPair, gbpUsdPair))
        .assertThrows(AppError.ClientFailure("oanda", "get-positions for EUR_USD has both sides open, hedging is not supported"))

    "ignore a position open on both sides when it is not requested" in
      currentOrdersWithHedgedEurUsd(NonEmptyList.of(gbpUsdPair)).asserting(_.map(_.currencyPair) mustBe List(gbpUsdPair))

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

    val filledOrderResponse = """{"order":{"id":"42","state":"FILLED","fillingTransactionID":"43"},"lastTransactionID":"43"}"""

    List(
      ("a server error", EntrySubmission.ServerError),
      ("a timeout", EntrySubmission.TimedOut),
      ("an unparseable created response", EntrySubmission.CreatedWith(""))
    ).foreach { case (description, submission) =>
      s"look up an entry order and its fill instead of resubmitting it after $description" in
        submitEnterWithUnknownOutcome(submission, filledOrderResponse)
          .asserting { case (result, submissions) =>
            result mustBe Right(OrderPlacementResult.filled(lookedUpEntryExecution))
            submissions must have size 1
          }
    }

    List("PENDING", "TRIGGERED").foreach { state =>
      s"return a pending entry referencing the order when an entry order with unknown outcome is $state" in
        submitEnterWithUnknownOutcome(EntrySubmission.ServerError, s"""{"order":{"id":"42","state":"$state"},"lastTransactionID":"43"}""")
          .asserting {
            case (Right(OrderPlacementResult.Pending(OrderRef(clientOrderId, Some("42")))), List(submission)) =>
              submission must include(clientOrderId)
            case result => fail(s"Expected a pending entry, received $result")
          }
    }

    "return cancelled when an entry order with unknown outcome is CANCELLED" in
      submitEnterWithUnknownOutcome(EntrySubmission.ServerError, """{"order":{"id":"42","state":"CANCELLED"},"lastTransactionID":"43"}""")
        .asserting(_._1 mustBe Right(OrderPlacementResult.Cancelled("Order 42 was cancelled")))

    "keep the fill of a looked-up entry order that has no filling transaction" in
      submitEnterWithUnknownOutcome(EntrySubmission.ServerError, """{"order":{"id":"42","state":"FILLED"},"lastTransactionID":"43"}""")
        .asserting(_._1 mustBe Right(OrderPlacementResult.FilledWithoutExecution(Some("42"))))

    List(
      ("an HTTP error", ("Unauthorized", StatusCode.Unauthorized)),
      ("an unparseable transaction", ("""{"transaction":{"id":"43"}}""", StatusCode.Ok))
    ).foreach { case (description, transactionResponse) =>
      s"keep the fill of a looked-up entry order when fetching its fill fails with $description" in
        submitEnterWithUnknownOutcome(EntrySubmission.ServerError, filledOrderResponse, transactionResponse = transactionResponse)
          .asserting { case (result, submissions) =>
            result mustBe Right(OrderPlacementResult.FilledWithoutExecution(Some("42")))
            submissions must have size 1
          }
    }

    "fail without resubmitting when an entry order with unknown outcome cannot be looked up" in
      submitEnterWithUnknownOutcome(EntrySubmission.TimedOut, "Unauthorized", StatusCode.Unauthorized)
        .asserting { case (result, submissions) =>
          result mustBe Left(AppError.ClientFailure("oanda", "get-order returned 401: Unauthorized"))
          submissions must have size 1
        }

    "fail without resubmitting when an entry order with unknown outcome was not created" in
      submitEnterWithUnknownOutcome(EntrySubmission.TimedOut, """{"errorMessage":"Order not found"}""", StatusCode.NotFound).asserting {
        case (Left(AppError.ClientFailure("oanda", message)), List(_)) => message must endWith("was not created")
        case result                                                    => fail(s"Expected a client failure, received $result")
      }

    "find the fills of every trade open on the requested side" in
      findEntryExecutions(TradeOrder.Position.Buy, tradeIds = List("40", "43")).asserting { case (executions, requestedTransactions) =>
        executions mustBe List(tradeOpeningExecution("40"), tradeOpeningExecution("43"))
        requestedTransactions mustBe List("40", "43")
      }

    "find no fills when the requested side has no trades" in
      findEntryExecutions(TradeOrder.Position.Sell, tradeIds = List("43")).asserting { case (executions, requestedTransactions) =>
        executions mustBe empty
        requestedTransactions mustBe empty
      }

    "find no fills when any transaction did not open its trade" in
      findEntryExecutions(TradeOrder.Position.Buy, tradeIds = List("40", "43"), notOpenedTradeIds = Set("43")).asserting(_._1 mustBe empty)

    "propagate a failure to retrieve any trade's fill" in
      findEntryExecutions(TradeOrder.Position.Buy, tradeIds = List("40", "43"), unavailableTradeIds = Set("43"))
        .assertThrows(AppError.ClientFailure("oanda", "get-transaction returned 401: Unauthorized"))

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
            ResponseStub.adjust(
              """{"orderCreateTransaction":{"id":"201","time":"2026-09-28T09:00:00Z","userID":123,"accountID":"123-456-789","batchID":"201"},
                |"relatedTransactionIDs":["201"],"lastTransactionID":"201"}""".stripMargin,
              StatusCode.Created
            )
          case _ => throw new RuntimeException("Unexpected request")
        }

      val liveParams: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = false, "123-456-789")
      val result                             = for
        client <- OandaBrokerClient.make[IO](config, testingBackend)
        status <- client.submit(liveParams, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0")))
      yield status

      result.asserting {
        case OrderPlacementResult.Pending(OrderRef(_, Some("201"))) => succeed
        case result                                                 => fail(s"Expected a pending entry, received $result")
      }
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

  private enum EntrySubmission:
    case TimedOut, ServerError
    case CreatedWith(body: String)

  private def submitEnterWithUnknownOutcome(
      submission: EntrySubmission,
      orderResponse: String,
      orderResponseStatus: StatusCode = StatusCode.Ok,
      transactionResponse: (String, StatusCode) = (fillTransactionResponse, StatusCode.Ok)
  ): IO[(Either[Throwable, OrderPlacementResult], List[String])] = {
    given Clock[IO]                                  = Clock.mock[IO](fillTime)
    val submissions                                  = AtomicReference(List.empty[String])
    def isSubmitted(orderSpecifier: String): Boolean =
      orderSpecifier.startsWith("@") && submissions.get.exists(_.contains(s""""clientExtensions":{"id":"${orderSpecifier.tail}"}"""))

    val testingBackend = fs2BackendStub
      .whenRequestMatchesPartial {
        case r if r.isGet && r.hasPath("/v3/accounts") =>
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        case r if r.isPost && r.hasPath("/v3/accounts/123-456-789/orders") =>
          submissions.updateAndGet(r.body.toString :: _)
          submission match
            case EntrySubmission.TimedOut          => throw new SocketTimeoutException("Read timed out")
            case EntrySubmission.ServerError       => ResponseStub.adjust("Server error", StatusCode.ServiceUnavailable)
            case EntrySubmission.CreatedWith(body) => ResponseStub.adjust(body, StatusCode.Created)
        case r if r.isGet && r.uri.path.init == List("v3", "accounts", "123-456-789", "orders") && isSubmitted(r.uri.path.last) =>
          ResponseStub.adjust(orderResponse, orderResponseStatus)
        case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/transactions/43") =>
          ResponseStub.adjust(transactionResponse._1, transactionResponse._2)
        case _ => throw new RuntimeException("Unexpected request")
      }

    for
      client <- OandaBrokerClient.make[IO](config, testingBackend)
      result <- client.submit(params, TradeOrder.Enter(TradeOrder.Position.Buy, eurUsdPair, BigDecimal(1), BigDecimal("1.0"))).attempt
    yield (result, submissions.get)
  }

  private def tradeOpeningExecution(tradeId: String): OrderExecution =
    OrderExecution(BigDecimal(s"1.10$tradeId"), fillTime.plusSeconds(1), BigDecimal(1), s"order-$tradeId", tradeId, List(tradeId))

  private def findEntryExecutions(
      position: TradeOrder.Position,
      tradeIds: List[String],
      notOpenedTradeIds: Set[String] = Set.empty,
      unavailableTradeIds: Set[String] = Set.empty
  ): IO[(List[OrderExecution], List[String])] = {
    val positionResponse = s"""{
      "position": {
        "instrument": "EUR_USD",
        "long": {"units": "100000", "tradeIDs": ${tradeIds
        .map(id => s""""$id"""")
        .mkString("[", ",", "]")}, "averagePrice": "1.1050", "unrealizedPL": "0.00"},
        "short": {"units": "0", "unrealizedPL": "0.00"}
      }
    }"""
    def transactionResponse(transactionId: String) =
      val openedTradeId = if notOpenedTradeIds.contains(transactionId) then "other" else transactionId
      if unavailableTradeIds.contains(transactionId) then ResponseStub.adjust("Unauthorized", StatusCode.Unauthorized)
      else ResponseStub.adjust(s"""{
          "transaction": {
            "id": "$transactionId", "time": "2026-09-28T09:00:01Z", "orderID": "order-$transactionId",
            "units": "100000", "price": "1.10$transactionId", "tradeOpened": {"tradeID": "$openedTradeId"}
          },
          "lastTransactionID": "$transactionId"
        }""")
    val transactionRequests = AtomicReference(List.empty[String])
    val testingBackend      = fs2BackendStub
      .whenRequestMatchesPartial {
        case r if r.isGet && r.hasPath("/v3/accounts") =>
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
          ResponseStub.adjust(positionResponse)
        case r if r.isGet && r.uri.path.init == List("v3", "accounts", "123-456-789", "transactions") =>
          transactionRequests.updateAndGet(_ :+ r.uri.path.last)
          transactionResponse(r.uri.path.last)
        case _ => throw new RuntimeException("Unexpected request")
      }

    for
      client     <- OandaBrokerClient.make[IO](config, testingBackend)
      executions <- client.findEntryExecutions(params, eurUsdPair, position)
    yield (executions, transactionRequests.get)
  }

  private def mapPositionSides(position: Json, adjustSide: JsonObject => JsonObject): Json =
    List("long", "short").foldLeft(position) { (json, side) =>
      json.hcursor.downField(side).withFocus(_.mapObject(adjustSide)).top.get
    }

  private def currentOrdersWithHedgedEurUsd(cps: NonEmptyList[CurrencyPair]): IO[List[OpenedTradeOrder]] = {
    val hedgedShort = Json.obj(
      "units"        -> Json.fromString("-30000"),
      "tradeIDs"     -> Json.arr(Json.fromString("789")),
      "averagePrice" -> Json.fromString("1.1100"),
      "unrealizedPL" -> Json.fromString("-5.00")
    )
    val response = parse(readJson("oanda/positions-success-response.json")).toOption.get.hcursor
      .downField("positions")
      .downArray
      .downField("short")
      .set(hedgedShort)
      .top
      .get
      .noSpaces
    val testingBackend = fs2BackendStub
      .whenRequestMatchesPartial {
        case r if r.isGet && r.hasPath("/v3/accounts") =>
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions") =>
          ResponseStub.adjust(response)
        case _ => throw new RuntimeException("Unexpected request")
      }

    OandaBrokerClient.make[IO](config, testingBackend).flatMap(_.getCurrentOrders(params, cps))
  }

  private def submitExit(
      longUnits: Int,
      shortUnits: Int,
      response: String,
      responseStatus: StatusCode = StatusCode.Ok,
      adjustSide: JsonObject => JsonObject = identity,
      expectClose: Boolean = true
  ): IO[OrderPlacementResult] = {
    val positionResponse = parse(s"""{
      "position": {
        "instrument": "EUR_USD",
        "long": {"units": "$longUnits", "unrealizedPL": "0.00"},
        "short": {"units": "$shortUnits", "unrealizedPL": "0.00"}
      }
    }""").toOption.get.hcursor
      .downField("position")
      .withFocus(position => mapPositionSides(position, adjustSide))
      .top
      .get
      .noSpaces
    val closeRequests  = AtomicReference(List.empty[String])
    val testingBackend = fs2BackendStub
      .whenRequestMatchesPartial {
        case r if r.isGet && r.hasPath("/v3/accounts") =>
          ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
        case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
          ResponseStub.adjust(positionResponse)
        case r if r.isPut && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD/close") =>
          closeRequests.updateAndGet(r.body.toString :: _)
          ResponseStub.adjust(response, responseStatus)
        case _ => throw new RuntimeException("Unexpected request")
      }

    for
      client <- OandaBrokerClient.make[IO](config, testingBackend)
      result <- client.submit(params, TradeOrder.Exit(eurUsdPair, BigDecimal(1))).attempt
      _      <- IO {
        if expectClose then {
          closeRequests.get must have size 1
          val expectedLong  = if longUnits == 0 then "NONE" else "ALL"
          val expectedShort = if shortUnits == 0 then "NONE" else "ALL"
          closeRequests.get.head must include(s""""longUnits":"$expectedLong"""")
          closeRequests.get.head must include(s""""shortUnits":"$expectedShort"""")
        } else closeRequests.get mustBe empty
      }
      status <- IO.fromEither(result)
    yield status
  }
}
