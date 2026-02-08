package currexx.clients.data.oanda

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Fs2HttpClient
import currexx.clients.data.MarketDataClient
import kirill5k.common.cats.{Cache, Clock}
import kirill5k.common.syntax.time.*
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import org.typelevel.log4cats.Logger
import io.circe.Codec
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.StatusCode

import java.time.Instant
import scala.concurrent.duration.*

private[clients] trait OandaDataClient[F[_]] extends MarketDataClient[F] with Fs2HttpClient[F]:
  def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]

final private class LiveOandaDataClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: OandaDataConfig,
    private val cache: Cache[F, (CurrencyPair, Interval), MarketTimeSeriesData]
)(using
    F: Temporal[F],
    logger: Logger[F],
    C: Clock[F]
) extends OandaDataClient[F] {
  import OandaDataClient.*

  override protected val name: String                                   = "oanda-data"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    for
      now            <- C.now
      timeSeriesData <- fetchTimeSeriesData(pair, interval, 150, now)
      result         <- filterIncompleteCandleIfNeeded(timeSeriesData, interval, now)
    yield result

  private def filterIncompleteCandleIfNeeded(
      data: MarketTimeSeriesData,
      interval: Interval,
      now: Instant
  ): F[MarketTimeSeriesData] =
    val firstCandle   = data.prices.head
    val candleEndTime = firstCandle.time.plus(interval.toDuration)
    val shouldExclude = candleEndTime.isAfter(now)

    if (shouldExclude && data.prices.size > 1) {
      F.pure(data.copy(prices = NonEmptyList.fromListUnsafe(data.prices.tail)))
    } else if (shouldExclude && data.prices.size == 1) {
      logger.warn(s"Only incomplete candle available for ${data.currencyPair} at ${data.interval}") >> F.pure(data)
    } else {
      F.pure(data)
    }

  override def latestPrice(pair: CurrencyPair): F[PriceRange] =
    C.now.flatMap { now =>
      fetchTimeSeriesData(pair, Interval.M1, 1, now).map(_.prices.head)
    }

  private def fetchTimeSeriesData(pair: CurrencyPair, interval: Interval, numOfTicks: Int, now: Instant): F[MarketTimeSeriesData] =
    cache.evalPutIfNew(pair -> interval) {
      val instrument  = s"${pair.base}_${pair.quote}"
      val granularity = interval match
        case Interval.M1  => "M1"
        case Interval.M5  => "M5"
        case Interval.M15 => "M15"
        case Interval.M30 => "M30"
        case Interval.H1  => "H1"
        case Interval.D1  => "D"

      dispatch {
        basicRequest
          .get(uri"${config.baseUri}/v3/instruments/$instrument/candles?granularity=$granularity&count=$numOfTicks&alignmentTimezone=UTC")
          .auth
          .bearer(config.apiKey)
          .response(asJson[CandlesResponse])
      }.flatMap { r =>
        r.body match
          case Right(res) =>
            MarketTimeSeriesData(pair, interval, res.prices, name).pure[F]
          case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
            logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
              F.raiseError(AppError.JsonParsingFailure(responseBody, s"Failed to parse $name response: ${error.getMessage}"))
          case Left(ResponseException.UnexpectedStatusCode(body, meta))
              if meta.code == StatusCode.Forbidden || meta.code == StatusCode.Unauthorized =>
            logger.error(s"$name-client/${meta.code.code}\n$body") >>
              F.raiseError(AppError.AccessDenied(s"$name authentication has expired or is invalid"))
          case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
            logger.error(s"$name-client/${meta.code.code}\n$body") >>
              F.sleep(delayBetweenConnectionFailures) >> fetchTimeSeriesData(pair, interval, numOfTicks, now)
      }
    }
}

private[clients] object OandaDataClient {

  final case class CandleMid(
      o: String,
      h: String,
      l: String,
      c: String
  ) derives Codec.AsObject

  final case class Candle(
      complete: Boolean,
      volume: Int,
      time: String,
      mid: CandleMid
  ) derives Codec.AsObject

  final case class CandlesResponse(
      instrument: String,
      granularity: String,
      candles: NonEmptyList[Candle]
  ) derives Codec.AsObject {
    def prices: NonEmptyList[PriceRange] =
      candles.map { c =>
        PriceRange(
          open = c.mid.o.toDouble,
          high = c.mid.h.toDouble,
          low = c.mid.l.toDouble,
          close = c.mid.c.toDouble,
          volume = c.volume.toDouble,
          time = Instant.parse(c.time)
        )
      }.reverse
  }

  def make[F[_]: {Temporal, Logger, Clock}](
      config: OandaDataConfig,
      fs2Backend: WebSocketStreamBackend[F, Fs2Streams[F]]
  ): F[OandaDataClient[F]] =
    Cache
      .make[F, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)
      .map(cache => LiveOandaDataClient(fs2Backend, config, cache))
}
