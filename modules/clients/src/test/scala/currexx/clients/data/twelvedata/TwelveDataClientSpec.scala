package currexx.clients.data.twelvedata

import cats.effect.{Deferred, IO, Ref, Temporal}
import cats.syntax.parallel.*
import currexx.clients.data.twelvedata.TwelveDataClient.CacheKey
import currexx.domain.errors.AppError
import currexx.domain.market.Currency.{EUR, USD}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import kirill5k.common.sttp.test.Sttp4WordSpec
import kirill5k.common.cats.{Cache, Clock}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.WebSocketStreamBackend
import sttp.client4.testing.ResponseStub
import sttp.model.StatusCode

import java.time.Instant
import scala.concurrent.duration.*

class TwelveDataClientSpec extends Sttp4WordSpec {

  given Logger[IO] = Slf4jLogger.getLogger[IO]

  val config = TwelveDataConfig("http://twelve-data.com", "api-key")
  val pair   = CurrencyPair(EUR, USD)

  "A TwelveDataClient" should {
    "reuse one successful fetch for overlapping requests with the same pair and interval" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

      val response = readJson("twelvedata/eur-usd-hourly-prices.response.json")
      val result   = for
        started <- Deferred[IO, Unit]
        release <- Deferred[IO, Unit]
        calls   <- Ref.of[IO, Int](0)
        backend = fs2BackendStub.whenAnyRequest.thenRespondF {
          calls.update(_ + 1) >> started.complete(()) >> release.get.as(ResponseStub.adjust(response))
        }
        client  <- makeClient(backend)
        results <- client
          .timeSeriesData(pair, Interval.H1)
          .start
          .bracket { first =>
            started.get >>
              List
                .fill(12)(client.timeSeriesData(pair, Interval.H1))
                .parSequence
                .start
                .bracket { waiting =>
                  for
                    _      <- IO.sleep(100.millis)
                    _      <- release.complete(())
                    head   <- first.joinWithNever
                    others <- waiting.joinWithNever
                  yield head :: others
                }(_.cancel)
          }(_.cancel)
        count <- calls.get
      yield (results, count)

      result.timeout(5.seconds).asserting { case (results, count) =>
        results must have size 13
        results.distinct must have size 1
        results.head.prices must have size 100
        count mustBe 1
      }
    }

    "serialize different requests for one pair while allowing other pairs to proceed" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

      val response  = readJson("twelvedata/eur-usd-hourly-prices.response.json")
      val otherPair = CurrencyPair(USD, EUR)
      val result    = for
        started      <- Deferred[IO, Unit]
        release      <- Deferred[IO, Unit]
        quoteStarted <- Deferred[IO, Unit]
        backend = fs2BackendStub
          .whenRequestMatches(_.hasParams(Map("symbol" -> "EUR/USD", "interval" -> "1h")))
          .thenRespondF(started.complete(()) >> release.get.as(ResponseStub.adjust(response)))
          .whenRequestMatches(_.hasParams(Map("symbol" -> "EUR/USD", "interval" -> "1min")))
          .thenRespondF(quoteStarted.complete(()).as(ResponseStub.adjust(response)))
          .whenRequestMatches(_.hasParams(Map("symbol" -> "USD/EUR")))
          .thenRespond(ResponseStub.adjust(response))
        client  <- makeClient(backend)
        blocked <- client
          .timeSeriesData(pair, Interval.H1)
          .start
          .bracket { first =>
            started.get >> client
              .latestPrice(pair)
              .start
              .bracket { quote =>
                for
                  _       <- client.latestPrice(otherPair)
                  _       <- IO.sleep(100.millis)
                  started <- quoteStarted.tryGet
                  _       <- release.complete(())
                  _       <- first.joinWithNever
                  _       <- quote.joinWithNever
                yield started.isEmpty
              }(_.cancel)
          }(_.cancel)
      yield blocked

      result.timeout(5.seconds).asserting(_ mustBe true)
    }

    "release the pair lock after a failed request" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

      val backend = fs2BackendStub.whenAnyRequest.thenRespondCyclic(
        ResponseStub.adjust("Forbidden", StatusCode.Forbidden),
        ResponseStub.adjust(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
      )
      val result = for
        client <- makeClient(backend)
        failed <- client.timeSeriesData(pair, Interval.H1).attempt
        data   <- client.timeSeriesData(pair, Interval.H1)
      yield (failed, data)

      result.timeout(5.seconds).asserting { case (failed, data) =>
        failed mustBe Left(AppError.AccessDenied("twelve-data authentication has expired"))
        data.prices must have size 100
      }
    }

    "release the pair lock when an in-flight request is cancelled" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

      val response = readJson("twelvedata/eur-usd-hourly-prices.response.json")
      val result   = for
        started <- Deferred[IO, Unit]
        calls   <- Ref.of[IO, Int](0)
        backend = fs2BackendStub.whenAnyRequest.thenRespondF {
          calls.getAndUpdate(_ + 1).flatMap {
            case 0 => started.complete(()) >> IO.never
            case _ => IO.pure(ResponseStub.adjust(response))
          }
        }
        client <- makeClient(backend)
        data   <- client
          .timeSeriesData(pair, Interval.H1)
          .start
          .bracket { first =>
            started.get >> first.cancel >> client.timeSeriesData(pair, Interval.H1)
          }(_.cancel)
        count <- calls.get
      yield (data, count)

      result.timeout(5.seconds).asserting { case (data, count) =>
        data.prices must have size 100
        count mustBe 2
      }
    }

    List(true, false).foreach { quoteFirst =>
      s"cache current quotes separately from M1 history when quoteFirst=$quoteFirst" in {
        given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T13:00:00Z"))

        val response      = readJson("twelvedata/eur-usd-hourly-prices.response.json")
        val quoteResponse = io.circe.parser
          .parse(response)
          .toOption
          .get
          .hcursor
          .downField("values")
          .withFocus(_.mapArray(_.take(1)))
          .top
          .get
          .noSpaces
        val result = for
          calls <- Ref.of[IO, Int](0)
          backend = fs2BackendStub
            .whenRequestMatches(_.hasParams(Map("outputsize" -> "1")))
            .thenRespondF(calls.update(_ + 1).as(ResponseStub.adjust(quoteResponse)))
            .whenRequestMatches(_.hasParams(Map("outputsize" -> "150")))
            .thenRespondF(calls.update(_ + 1).as(ResponseStub.adjust(response)))
          client <- makeClient(backend)
          _      <- if quoteFirst then client.latestPrice(pair).void else client.timeSeriesData(pair, Interval.M1).void
          data   <- client.timeSeriesData(pair, Interval.M1)
          _      <- client.latestPrice(pair)
          count  <- calls.get
        yield (data, count)

        result.timeout(5.seconds).asserting { case (data, count) =>
          data.prices must have size 100
          count mustBe 2
        }
      }
    }

    "retrieve time-series data" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2022-11-08T10:15:30Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1day",
        "apikey"     -> "api-key",
        "outputsize" -> "150",
        "timezone"   -> "UTC"
      )

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(readJson("twelvedata/eur-usd-daily-prices.response.json"))
          case _ => throw new RuntimeException()
        }

      val result = makeClient(testingBackend).flatMap(_.timeSeriesData(pair, Interval.D1))

      result.asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.0424, 1.0463, 1.0418, 1.0452, 0.0, Instant.parse("2022-07-03T00:00:00Z"))
      }
    }

    "retry after some delay in case of api limit error" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2022-11-08T10:15:30Z"))

      val testingBackend = fs2BackendStub.whenAnyRequest
        .thenRespondCyclic(
          ResponseStub.adjust(readJson("twelvedata/limit-error.json")),
          ResponseStub.adjust(readJson("twelvedata/eur-usd-daily-prices.response.json"))
        )

      val result = makeClient(testingBackend).flatMap(_.timeSeriesData(pair, Interval.D1))

      result.timeout(5.seconds).asserting { timeSeriesData =>
        timeSeriesData.currencyPair mustBe pair
        timeSeriesData.interval mustBe Interval.D1
        timeSeriesData.prices must have size 100
        timeSeriesData.prices.head mustBe PriceRange(0.99105, 1.0034, 0.9899, 1.0025, 0.0, Instant.parse(s"2022-11-07T00:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.0424, 1.0463, 1.0418, 1.0452, 0.0, Instant.parse("2022-07-03T00:00:00Z"))
      }
    }

    List(
      (
        "exclude the incomplete current candle before keeping the latest 100 candles",
        Instant.parse("2026-01-19T12:03:00Z"),
        PriceRange(1.16246, 1.16294, 1.16209, 1.16285, 0.0, Instant.parse("2026-01-19T11:00:00Z")),
        PriceRange(1.16296, 1.16355, 1.16266, 1.16316, 0.0, Instant.parse("2026-01-15T08:00:00Z"))
      ),
      (
        "retrieve the latest 100 completed candles in reverse chronological order",
        Instant.parse("2026-01-19T13:00:00Z"),
        PriceRange(1.16284, 1.16284, 1.16269, 1.16272, 0.0, Instant.parse("2026-01-19T12:00:00Z")),
        PriceRange(1.16316, 1.16399, 1.16311, 1.16362, 0.0, Instant.parse("2026-01-15T09:00:00Z"))
      )
    ).foreach { case (description, now, expectedFirst, expectedLast) =>
      description in {
        given Clock[IO] = Clock.mock[IO](now)

        val requestParams = Map(
          "symbol"     -> "EUR/USD",
          "interval"   -> "1h",
          "apikey"     -> "api-key",
          "outputsize" -> "150",
          "timezone"   -> "UTC"
        )

        val testingBackend = fs2BackendStub
          .whenRequestMatchesPartial {
            case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
              ResponseStub.adjust(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
            case _ => throw new RuntimeException()
          }

        val result = makeClient(testingBackend).flatMap(_.timeSeriesData(pair, Interval.H1))

        result.asserting { timeSeriesData =>
          timeSeriesData.currencyPair mustBe pair
          timeSeriesData.interval mustBe Interval.H1
          timeSeriesData.prices must have size 100
          timeSeriesData.prices.head mustBe expectedFirst
          timeSeriesData.prices.last mustBe expectedLast
          val times = timeSeriesData.prices.toList.map(_.time)
          times mustBe times.sortWith(_.isAfter(_))
        }
      }
    }

    "use configured fetch and signal candle counts" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:03:00Z"))

      val customConfig  = config.copy(fetchCandleCount = 120, signalCandleCount = 80)
      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1h",
        "apikey"     -> "api-key",
        "outputsize" -> "120",
        "timezone"   -> "UTC"
      )
      val response = io.circe.parser
        .parse(readJson("twelvedata/eur-usd-hourly-prices.response.json"))
        .toOption
        .get
        .hcursor
        .downField("values")
        .withFocus(_.mapArray(_.take(120)))
        .top
        .get
        .noSpaces

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = makeClient(testingBackend, customConfig).flatMap(_.timeSeriesData(pair, Interval.H1))

      result.asserting { timeSeriesData =>
        timeSeriesData.prices must have size 80
        timeSeriesData.prices.head mustBe PriceRange(1.16246, 1.16294, 1.16209, 1.16285, 0.0, Instant.parse("2026-01-19T11:00:00Z"))
        timeSeriesData.prices.last mustBe PriceRange(1.1612, 1.16142, 1.16101, 1.16113, 0.0, Instant.parse("2026-01-16T04:00:00Z"))
      }
    }

    "reject invalid configured candle counts" in {
      an[IllegalArgumentException] must be thrownBy config.copy(signalCandleCount = 0)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 100)
      an[IllegalArgumentException] must be thrownBy config.copy(fetchCandleCount = 99)
    }

    "retrieve the current quote without filtering an incomplete candle" in {
      given Clock[IO] = Clock.mock[IO](Instant.parse("2026-01-19T12:00:30Z"))

      val requestParams = Map(
        "symbol"     -> "EUR/USD",
        "interval"   -> "1min",
        "apikey"     -> "api-key",
        "outputsize" -> "1",
        "timezone"   -> "UTC"
      )
      val response = """{
        "meta": {"symbol": "EUR/USD", "interval": "1min"},
        "values": [{
          "datetime": "2026-01-19 12:00:00",
          "open": "1.16284",
          "high": "1.16284",
          "low": "1.16269",
          "close": "1.16272"
        }]
      }"""

      val testingBackend = fs2BackendStub
        .whenRequestMatchesPartial {
          case r if r.isGet && r.isGoingTo("twelve-data.com/time_series") && r.hasParams(requestParams) =>
            ResponseStub.adjust(response)
          case _ => throw new RuntimeException()
        }

      val result = makeClient(testingBackend).flatMap(_.latestPrice(pair))

      result.asserting { price =>
        price mustBe PriceRange(1.16284, 1.16284, 1.16269, 1.16272, 0.0, Instant.parse("2026-01-19T12:00:00Z"))
      }
    }
  }

  private def makeClient(
      backend: WebSocketStreamBackend[IO, Fs2Streams[IO]],
      clientConfig: TwelveDataConfig = config
  )(using marketClock: Clock[IO]): IO[TwelveDataClient[IO]] =
    for
      // Cache expiry uses real time; the scenario clock controls candle completion.
      cache  <- Cache.make[IO, CacheKey, MarketTimeSeriesData](3.minutes, 15.seconds)(using Temporal[IO], Clock.make[IO])
      client <- TwelveDataClient.make[IO](clientConfig, backend, cache, 100.millis)
    yield client
}
