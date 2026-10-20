package currexx.clients.broker.oanda

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.clients.broker.BrokerParameters
import currexx.domain.market.Currency.{EUR, USD}
import currexx.domain.market.{CurrencyPair, OrderExecution, OrderPlacementResult, TradeOrder}
import io.circe.Json
import io.circe.parser.parse
import kirill5k.common.cats.Clock
import kirill5k.common.sttp.test.Sttp4WordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

import java.time.Instant
import java.util.concurrent.atomic.AtomicReference
import scala.concurrent.duration.*

class OandaBrokerRetrySpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  private val config                         = OandaBrokerConfig("https://api-fxpractice.oanda.com", "https://api-fxtrade.oanda.com")
  private val params: BrokerParameters.Oanda = BrokerParameters.Oanda("test-api-key", demo = true, "123-456-789")
  private val pair                           = CurrencyPair(EUR, USD)
  private val fillTime                       = Instant.parse("2026-09-28T09:00:00Z")
  private val halted                         = OrderPlacementResult.Cancelled("MARKET_HALTED")
  private val longFill                       = OrderExecution(BigDecimal("1.1055"), fillTime, BigDecimal(1), "201", "202", List("123"))
  private val shortFill                      = OrderExecution(BigDecimal("1.1057"), fillTime, BigDecimal(1), "203", "204", List("124"))
  private val entryFill                      = OrderExecution(BigDecimal("1.1050"), fillTime, BigDecimal(1), "201", "202", List("202"))

  "An OandaBrokerClient retrying market halts" should {
    "wait three minutes before retrying an entry with a fresh client order ID" in
      submitEntry(List(entryResponse(Some("MARKET_HALTED")), entryResponse(filled = true))).asserting { trace =>
        trace.result mustBe OrderPlacementResult.filled(entryFill)
        trace.events mustBe List("submit", "sleep", "submit")
        trace.sleeps mustBe List(3.minutes)
        trace.submissions must have size 2
        trace.submissions.distinct must have size 2
      }

    "stop a halted entry after three additional attempts by default" in
      submitEntry(List(entryResponse(Some("MARKET_HALTED")))).asserting { trace =>
        trace.result mustBe halted
        trace.sleeps mustBe List.fill(3)(3.minutes)
        trace.submissions must have size 4
        trace.submissions.distinct must have size 4
      }

    "return other entry cancellations without retrying" in
      submitEntry(List(entryResponse(Some("INSUFFICIENT_MARGIN")))).asserting { trace =>
        trace.result mustBe OrderPlacementResult.Cancelled("INSUFFICIENT_MARGIN")
        trace.sleeps mustBe empty
        trace.submissions must have size 1
      }

    "avoid resubmitting an entry response that contains both a fill and a market halt" in
      submitEntry(List(entryResponse(Some("MARKET_HALTED"), filled = true))).asserting { trace =>
        trace.result mustBe halted
        trace.sleeps mustBe empty
        trace.submissions must have size 1
      }

    "re-fetch the original position after waiting three minutes and retry its exit" in
      submitExit(
        List(closeResponse("longOrderCancelTransaction" -> cancellation()), closeResponse("longOrderFillTransaction" -> fill("long")))
      )
        .asserting { trace =>
          trace.result mustBe OrderPlacementResult.filled(longFill)
          trace.events mustBe List("position", "submit", "sleep", "position", "submit")
          trace.sleeps mustBe List(3.minutes)
          trace.submissions must have size 2
        }

    "stop a halted exit after three additional attempts by default" in
      submitExit(List(closeResponse("longOrderCancelTransaction" -> cancellation()))).asserting { trace =>
        trace.result mustBe halted
        trace.sleeps mustBe List.fill(3)(3.minutes)
        trace.events.count(_ == "position") mustBe 4
        trace.submissions must have size 4
      }

    "retry both sides of a position when every cancellation is a market halt" in {
      val cancelled = closeResponse("longOrderCancelTransaction" -> cancellation(), "shortOrderCancelTransaction" -> cancellation())
      val filled    = closeResponse("longOrderFillTransaction" -> fill("long"), "shortOrderFillTransaction" -> fill("short"))
      val position  = positionResponse(shortUnits = -100000, shortIds = Some(List("124")))

      submitExit(List(cancelled, filled), List(position)).asserting { trace =>
        trace.result mustBe OrderPlacementResult.Filled(NonEmptyList.of(longFill, shortFill))
        trace.sleeps mustBe List(3.minutes)
        trace.submissions.foreach { body =>
          body must include(""""longUnits":"ALL"""")
          body must include(""""shortUnits":"ALL"""")
        }
        trace.submissions must have size 2
      }
    }

    "return a close cancellation immediately when any reason is not a market halt" in {
      val cancelled = closeResponse(
        "longOrderCancelTransaction"  -> cancellation(),
        "shortOrderCancelTransaction" -> cancellation("FOK_ORDER_IMMEDIATE_PARTIAL_FILL")
      )
      val position = positionResponse(shortUnits = -100000, shortIds = Some(List("124")))

      submitExit(List(cancelled), List(position)).asserting { trace =>
        trace.result mustBe OrderPlacementResult.Cancelled("MARKET_HALTED; FOK_ORDER_IMMEDIATE_PARTIAL_FILL")
        trace.sleeps mustBe empty
        trace.submissions must have size 1
      }
    }

    "avoid repeating a close when one side filled and another was halted" in {
      val response = closeResponse("longOrderFillTransaction" -> fill("long"), "shortOrderCancelTransaction" -> cancellation())
      val position = positionResponse(shortUnits = -100000, shortIds = Some(List("124")))

      submitExit(List(response), List(position)).asserting { trace =>
        trace.result mustBe halted
        trace.sleeps mustBe empty
        trace.submissions must have size 1
      }
    }

    "return no position if the position closes during the retry delay" in
      submitExit(
        List(closeResponse("longOrderCancelTransaction" -> cancellation())),
        List(positionResponse(), readJson("oanda/closed-position-response.json"))
      ).asserting { trace =>
        trace.result mustBe OrderPlacementResult.NoPosition
        trace.events mustBe List("position", "submit", "sleep", "position")
        trace.sleeps mustBe List(3.minutes)
        trace.submissions must have size 1
      }

    List(
      "replacement trades" -> positionResponse(longIds = Some(List("999"))),
      "additional trades"  -> positionResponse(longIds = Some(List("123", "999"))),
      "the opposite side"  -> positionResponse(longUnits = 0, longIds = None, shortUnits = -100000, shortIds = Some(List("124"))),
      "missing trade IDs"  -> positionResponse(longIds = None)
    ).foreach { case (description, changedPosition) =>
      s"preserve the cancellation without closing $description after the delay" in
        submitExit(List(closeResponse("longOrderCancelTransaction" -> cancellation())), List(positionResponse(), changedPosition))
          .asserting { trace =>
            trace.result mustBe halted
            trace.events mustBe List("position", "submit", "sleep", "position")
            trace.sleeps mustBe List(3.minutes)
            trace.submissions must have size 1
          }
    }

    List(None, Some(List.empty[String])).foreach { ids =>
      s"avoid retrying a close when its original position has trade IDs $ids" in
        submitExit(List(closeResponse("longOrderCancelTransaction" -> cancellation())), List(positionResponse(longIds = ids)))
          .asserting { trace =>
            trace.result mustBe halted
            trace.events mustBe List("position", "submit", "sleep", "position")
            trace.sleeps mustBe List(3.minutes)
            trace.submissions must have size 1
          }
    }
  }

  private case class Trace(
      result: OrderPlacementResult,
      sleeps: List[FiniteDuration],
      submissions: List[String],
      events: List[String]
  )

  private def entryResponse(reason: Option[String] = None, filled: Boolean = false): String = {
    val transaction = parse("""{
      "id":"201", "time":"2026-09-28T09:00:00Z", "userID":123, "accountID":"123-456-789", "batchID":"201"
    }""").toOption.get
    val cancellation = reason.map(value =>
      "orderCancelTransaction" -> transaction.deepMerge(
        Json.obj("id" -> Json.fromString("203"), "type" -> Json.fromString("ORDER_CANCEL"), "reason" -> Json.fromString(value))
      )
    )
    val fill = Option.when(filled)(
      "orderFillTransaction" -> parse("""{
        "id":"202", "orderID":"201", "time":"2026-09-28T09:00:00Z",
        "units":"100000", "price":"1.1050", "tradeOpened":{"tradeID":"202"}
      }""").toOption.get
    )
    Json
      .obj(
        (List(
          "orderCreateTransaction" -> transaction,
          "relatedTransactionIDs"  -> Json.arr(Json.fromString("201"), Json.fromString("202")),
          "lastTransactionID"      -> Json.fromString("202")
        ) ++ cancellation.toList ++ fill.toList)*
      )
      .noSpaces
  }

  private def cancellation(reason: String = "MARKET_HALTED"): Json =
    Json.obj("reason" -> Json.fromString(reason))

  private def fill(side: String): Json =
    parse(readJson("oanda/close-position-success-response.json")).toOption.get.hcursor.downField(s"${side}OrderFillTransaction").focus.get

  private def closeResponse(transactions: (String, Json)*): String =
    Json.obj((transactions :+ ("lastTransactionID" -> Json.fromString("204")))*).noSpaces

  private def positionResponse(
      longUnits: Int = 100000,
      longIds: Option[List[String]] = Some(List("123")),
      shortUnits: Int = 0,
      shortIds: Option[List[String]] = None
  ): String =
    List(("long", longUnits, longIds), ("short", shortUnits, shortIds))
      .foldLeft(parse(readJson("oanda/position-success-response.json")).toOption.get) { case (position, (side, units, ids)) =>
        position.hcursor
          .downField("position")
          .downField(side)
          .withFocus(
            _.mapObject { obj =>
              val withUnits = obj.add("units", Json.fromString(units.toString))
              ids.fold(withUnits.remove("tradeIDs"))(values => withUnits.add("tradeIDs", Json.arr(values.map(Json.fromString)*)))
            }
          )
          .top
          .get
      }
      .noSpaces

  private def submitEntry(responses: List[String]): IO[Trace] =
    submit(TradeOrder.Enter(TradeOrder.Position.Buy, pair, BigDecimal(1), BigDecimal(1)), responses, Nil)

  private def submitExit(responses: List[String], positions: List[String] = List(positionResponse())): IO[Trace] =
    submit(TradeOrder.Exit(pair, BigDecimal(1)), responses, positions)

  private def submit(order: TradeOrder, responses: List[String], positions: List[String]): IO[Trace] = {
    val sleeps      = AtomicReference(List.empty[FiniteDuration])
    val submissions = AtomicReference(List.empty[String])
    val events      = AtomicReference(List.empty[String])
    val reads       = AtomicReference(0)
    given Clock[IO] = new Clock[IO] {
      override def now: IO[Instant]                                          = IO.pure(fillTime)
      override def durationBetweenNowAnd(other: Instant): IO[FiniteDuration] =
        IO.pure(java.time.Duration.between(fillTime, other).toNanos.nanos)
      override def sleep(duration: FiniteDuration): IO[Unit] = IO {
        sleeps.updateAndGet(_ :+ duration)
        events.updateAndGet(_ :+ "sleep")
        ()
      }
    }
    val backend = fs2BackendStub.whenRequestMatchesPartial {
      case r if r.isGet && r.hasPath("/v3/accounts") =>
        ResponseStub.adjust(readJson("oanda/accounts-success-response.json"))
      case r if r.isGet && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD") =>
        events.updateAndGet(_ :+ "position")
        val index = reads.getAndUpdate(_ + 1)
        ResponseStub.adjust(positions(math.min(index, positions.size - 1)))
      case r
          if (r.isPost && r.hasPath("/v3/accounts/123-456-789/orders")) ||
            (r.isPut && r.hasPath("/v3/accounts/123-456-789/positions/EUR_USD/close")) =>
        events.updateAndGet(_ :+ "submit")
        val index = submissions.getAndUpdate(_ :+ r.body.toString).size
        ResponseStub.adjust(responses(math.min(index, responses.size - 1)), if r.isPost then StatusCode.Created else StatusCode.Ok)
      case _ => throw new RuntimeException("Unexpected request")
    }

    for
      client <- OandaBrokerClient.make[IO](config, backend)
      result <- client.submit(params, order)
    yield Trace(result, sleeps.get, submissions.get, events.get)
  }
}
