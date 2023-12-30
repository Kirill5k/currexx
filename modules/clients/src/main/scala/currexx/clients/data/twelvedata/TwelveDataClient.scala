package currexx.clients.data.twelvedata

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.HttpClient
import currexx.clients.data.MarketDataClient
import currexx.domain.cache.Cache
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import org.typelevel.log4cats.Logger
import io.circe.Codec
import sttp.client3.circe.asJson
import sttp.client3.*
import sttp.model.StatusCode

import java.time.temporal.ChronoUnit
import java.time.{Instant, LocalDate, LocalTime, ZoneOffset}
import scala.concurrent.duration.*

private[clients] trait TwelveDataClient[F[_]] extends MarketDataClient[F] with HttpClient[F]:
  def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]

final private class LiveTwelveDataClient[F[_]](
    private val config: TwelveDataConfig,
    override protected val backend: SttpBackend[F, Any],
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
    cache
      .get(pair -> interval)
      .flatMap {
        case Some(data) => data.pure[F]
        case None =>
          val sym = s"${pair.base}/${pair.quote}"
          val int = s"${interval.number}${if (interval.unit == "hour") "h" else interval.unit.slice(0, 3)}"
          val uri = uri"${config.baseUri}/time_series?symbol=$sym&interval=$int&apikey=${config.apiKey}&outputsize=$numOfTicks"
          dispatch(basicRequest.get(uri).response(asJson[TimeSeriesResponse]))
            .flatMap { r =>
              r.body match
                case Right(res) =>
                  val prices =
                    res.values.zipWithIndex.map((v, i) => PriceRange(v.open, v.high, v.low, v.close, 0d, v.datetime.toInstant(i)))
                  MarketTimeSeriesData(pair, interval, prices).pure[F]
                case Left(DeserializationException(responseBody, error)) =>
                  if (responseBody.matches(".*\"code\":( )?429.*"))
                    F.sleep(delayBetweenClientFailures) >> fetchTimeSeriesData(pair, interval, numOfTicks)
                  else
                    logger.error(s"$name-client/json-parsing: ${error.getMessage}\n$responseBody") >>
                      F.raiseError(AppError.JsonParsingFailure(responseBody, s"Failed to parse $name response: ${error.getMessage}"))
                case Left(HttpError(responseBody, StatusCode.Forbidden)) =>
                  logger.error(s"$name-client/forbidden\n$responseBody") >>
                    F.raiseError(AppError.AccessDenied(s"$name authentication has expired"))
                case Left(HttpError(responseBody, status)) =>
                  logger.error(s"$name-client/${status.code}\n$responseBody") >>
                    F.sleep(delayBetweenConnectionFailures) >> fetchTimeSeriesData(pair, interval, numOfTicks)
            }
            .flatTap(data => cache.put(pair -> interval, data))
      }

  extension (dateString: String)
    def toInstant(i: Int): Instant =
      if (dateString.length == 10 && i == 0)
        LocalDate.parse(dateString).atTime(LocalTime.now().truncatedTo(ChronoUnit.MINUTES)).toInstant(ZoneOffset.UTC)
      else if (dateString.length == 10)
        LocalDate.parse(dateString).atStartOfDay().toInstant(ZoneOffset.UTC)
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

  def make[F[_]: Temporal: Logger](
      config: TwelveDataConfig,
      backend: SttpBackend[F, Any],
      delayBetweenClientFailures: FiniteDuration = 1.minute
  ): F[TwelveDataClient[F]] =
    Cache
      .make[F, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)
      .map(cache => LiveTwelveDataClient(config, backend, delayBetweenClientFailures, cache))
}
