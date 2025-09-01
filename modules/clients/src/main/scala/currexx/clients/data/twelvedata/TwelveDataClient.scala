package currexx.clients.data.twelvedata

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.Fs2HttpClient
import currexx.clients.data.MarketDataClient
import kirill5k.common.cats.Cache
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

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}
import scala.concurrent.duration.*

private[clients] trait TwelveDataClient[F[_]] extends MarketDataClient[F] with Fs2HttpClient[F]:
  def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]

final private class LiveTwelveDataClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: TwelveDataConfig,
    private val delayBetweenClientFailures: FiniteDuration,
    private val cache: Cache[F, (CurrencyPair, Interval), MarketTimeSeriesData]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends TwelveDataClient[F] {
  import TwelveDataClient.*

  override protected val name: String                                   = "twelve-data"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    fetchTimeSeriesData(pair, interval, 150)

  override def latestPrice(pair: CurrencyPair): F[PriceRange] =
    fetchTimeSeriesData(pair, Interval.M1, 1).map(_.prices.head)

  private def fetchTimeSeriesData(pair: CurrencyPair, interval: Interval, numOfTicks: Int): F[MarketTimeSeriesData] =
    cache.evalPutIfNew(pair -> interval) {
      val sym = s"${pair.base}/${pair.quote}"
      val int = s"${interval.number}${if (interval.unit == "hour") "h" else interval.unit.slice(0, 3)}"
      val uri = uri"${config.baseUri}/time_series?symbol=$sym&interval=$int&apikey=${config.apiKey}&outputsize=$numOfTicks&timezone=UTC"
      dispatch(basicRequest.get(uri).response(asJson[TimeSeriesResponse]))
        .flatMap { r =>
          r.body match
            case Right(res) =>
              MarketTimeSeriesData(pair, interval, res.priceRanges).pure[F]
            case Left(ResponseException.DeserializationException(b, _, _)) if b.matches(".*\"code\":( )?429.*") =>
              F.sleep(delayBetweenClientFailures) >> fetchTimeSeriesData(pair, interval, numOfTicks)
            case Left(ResponseException.DeserializationException(responseBody, error, _)) =>
              logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
                F.raiseError(AppError.JsonParsingFailure(responseBody, s"Failed to parse $name response: ${error.getMessage}"))
            case Left(ResponseException.UnexpectedStatusCode(body, meta)) if meta.code == StatusCode.Forbidden =>
              logger.error(s"$name-client/forbidden\n$body") >>
                F.raiseError(AppError.AccessDenied(s"$name authentication has expired"))
            case Left(ResponseException.UnexpectedStatusCode(body, meta)) =>
              logger.error(s"$name-client/${meta.code.code}\n$body") >>
                F.sleep(delayBetweenConnectionFailures) >> fetchTimeSeriesData(pair, interval, numOfTicks)
        }
    }

  extension (ts: TimeSeriesResponse)
    private def priceRanges: NonEmptyList[PriceRange] =
      ts.values.zipWithIndex.map((v, i) => PriceRange(v.open, v.high, v.low, v.close, 0d, v.datetime.toInstant(i)))

  extension (dateString: String)
    private def toInstant(i: Int): Instant =
      if (dateString.length == 10 && i == 0)
        LocalDate.parse(dateString).atTime(LocalTime.now().truncatedTo(ChronoUnit.MINUTES)).toInstant(ZoneOffset.UTC)
      else if (dateString.length == 10)
        LocalDate.parse(dateString).toInstantAtStartOfDay
      else
        Instant.parse(s"${dateString.replaceFirst(" ", "T")}Z")
}

private[clients] object TwelveDataClient {

  final case class TimeSeriesMeta(
      symbol: String,
      interval: String
  ) derives Codec.AsObject

  final case class TimeSeriesValue(
      datetime: String,
      open: Double,
      high: Double,
      low: Double,
      close: Double
  ) derives Codec.AsObject

  final case class TimeSeriesResponse(
      meta: TimeSeriesMeta,
      values: NonEmptyList[TimeSeriesValue]
  ) derives Codec.AsObject

  def make[F[_]: {Temporal, Logger}](
      config: TwelveDataConfig,
      fs2Backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      delayBetweenClientFailures: FiniteDuration = 1.minute
  ): F[TwelveDataClient[F]] =
    Cache
      .make[F, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)
      .map(cache => LiveTwelveDataClient(fs2Backend, config, delayBetweenClientFailures, cache))
}
