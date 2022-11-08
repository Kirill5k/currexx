package currexx.clients.data.twelvedata

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
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
    val sym = s"${pair.base}/${pair.quote}"
    val int = s"${interval.number}/${if (interval.unit == "hour") "h" else interval.unit.slice(0, 3)}"
    val uri = uri"${config.baseUri}/time_series?symbol=$sym&interval=$int&apikey=${config.apiKey}&outputsize=150"
    dispatch(basicRequest.get(uri).response(asJson[TimeSeriesResponse]))
      .flatMap { r =>
        r.body match {
          case Right(res) =>
            val prices = res.values.zipWithIndex.map((v, i) => PriceRange(v.open, v.high, v.low, v.close, 0d, v.datetime.toInstant(i)))
            MarketTimeSeriesData(pair, interval, prices).pure[F]
          case Left(DeserializationException(_, _)) =>
            F.sleep(1.minute) >>  timeSeriesData(pair, interval)
          case Left(HttpError(responseBody, StatusCode.Forbidden)) =>
            logger.error(s"$name-client/forbidden\n$responseBody") >>
              F.raiseError(AppError.AccessDenied(s"$name authentication has expired"))
          case Left(HttpError(responseBody, status)) =>
            logger.error(s"$name-client/${status.code}\n$responseBody") >>
              F.sleep(delayBetweenConnectionFailures) >> timeSeriesData(pair, interval)
        }
      }

  extension (ld: LocalDate)
    def toInstant(i: Int): Instant =
      if (i > 0) ld.atStartOfDay().toInstant(ZoneOffset.UTC) else Instant.now

  override def latestPrice(currencyPair: CurrencyPair): F[PriceRange] = ???
}

private[clients] object TwelveDataClient {

  final case class TimeSeriesMeta(
      symbol: String,
      interval: String
  ) derives Codec.AsObject

  final case class TimeSeriesValue(
      datetime: LocalDate,
      open: Double,
      high: Double,
      low: Double,
      close: Double
  ) derives Codec.AsObject

  final case class TimeSeriesResponse(
      meta: TimeSeriesMeta,
      values: NonEmptyList[TimeSeriesValue]
  ) derives Codec.AsObject
}