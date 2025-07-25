package currexx.clients.data.alphavantage

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.applicativeError.*
import currexx.clients.data.MarketDataClient
import currexx.clients.Fs2HttpClient
import kirill5k.common.cats.Cache
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import io.circe.{Codec, JsonObject}
import org.typelevel.log4cats.Logger
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.*
import sttp.client4.circe.asJson
import sttp.client4.WebSocketStreamBackend
import sttp.model.{StatusCode, Uri}

import scala.concurrent.duration.*

private[clients] trait AlphaVantageClient[F[_]] extends MarketDataClient[F] with Fs2HttpClient[F]:
  def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData]

final private class LiveAlphaVantageClient[F[_]](
    override protected val backend: WebSocketStreamBackend[F, Fs2Streams[F]],
    private val config: AlphaVantageConfig,
    private val delayBetweenClientFailures: FiniteDuration,
    private val cache: Cache[F, (CurrencyPair, Interval), MarketTimeSeriesData]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends AlphaVantageClient[F]:

  override protected val name: String                                   = "alpha-vantage"
  override protected val delayBetweenConnectionFailures: FiniteDuration = 5.seconds

  override def latestPrice(pair: CurrencyPair): F[PriceRange] =
    timeSeriesData(pair, Interval.D1).map(_.prices.head)

  override def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    cache.evalPutIfNew(pair -> interval)(fetchTimeSeriesData(pair, interval))

  private def fetchTimeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    interval match
      case Interval.D1 => dailyTimeSeriesData(pair)
      case _           => intradayTimeSeriesData(pair, interval)

  private def dailyTimeSeriesData(pair: CurrencyPair): F[MarketTimeSeriesData] = {
    val params = Map(
      "function"    -> "FX_DAILY",
      "from_symbol" -> pair.base.code,
      "to_symbol"   -> pair.quote.code,
      "apikey"      -> config.apiKey
    )
    sendRequest(uri"${config.baseUri}/query?$params", ResponseMapper.mapDailyTimeSeriesData)
      .map(prs => MarketTimeSeriesData(pair, Interval.D1, prs))
  }

  private def intradayTimeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] = {
    val params = Map(
      "function"    -> "FX_INTRADAY",
      "from_symbol" -> pair.base.code,
      "to_symbol"   -> pair.quote.code,
      "apikey"      -> config.apiKey,
      "interval"    -> (interval match
        case Interval.M1  => "1min"
        case Interval.M5  => "5min"
        case Interval.M15 => "15min"
        case Interval.M30 => "30min"
        case Interval.H1  => "60min"
        case _            => "")
    )
    sendRequest(uri"${config.baseUri}/query?$params", ResponseMapper.mapIntradayTimeSeriesData)
      .map(prs => MarketTimeSeriesData(pair, interval, prs))
  }

  private def sendRequest(
      uri: Uri,
      mapper: JsonObject => Either[AppError, NonEmptyList[PriceRange]],
      attempt: Int = 0
  ): F[NonEmptyList[PriceRange]] =
    dispatch(basicRequest.get(uri).response(asJson[JsonObject]))
      .flatMap { r =>
        r.body match {
          case Right(value) =>
            F.fromEither(mapper(value))
          case Left(ResponseException.UnexpectedStatusCode(responseBody, meta)) if meta.code == StatusCode.Forbidden =>
            logger.error(s"$name-client/forbidden\n$responseBody") >>
              F.raiseError(AppError.AccessDenied(s"$name authentication has expired"))
          case Left(ResponseException.UnexpectedStatusCode(responseBody, meta)) =>
            logger.error(s"$name-client/${meta.code.code}\n$responseBody") >>
              F.sleep(delayBetweenConnectionFailures) >> sendRequest(uri, mapper)
          case Left(error) =>
            logger.error(s"$name-client/error\n$error") >>
              F.sleep(delayBetweenConnectionFailures) >> sendRequest(uri, mapper)
        }
      }
      .handleErrorWith { error =>
        if (attempt < 5) F.sleep(delayBetweenClientFailures) >> sendRequest(uri, mapper, attempt + 1)
        else F.raiseError(error)
      }

private[clients] object AlphaVantageClient {

  final case class OHLC(
      `1. open`: Double,
      `2. high`: Double,
      `3. low`: Double,
      `4. close`: Double
  ) derives Codec.AsObject

  final case class DailyResponseMetadata(
      `5. Last Refreshed`: String,
      `6. Time Zone`: String
  ) derives Codec.AsObject

  final case class DailyTimeSeriesResponse(
      `Meta Data`: DailyResponseMetadata,
      `Time Series FX (Daily)`: Map[String, OHLC]
  ) derives Codec.AsObject

  final case class IntradayResponseMetadata(
      `4. Last Refreshed`: String,
      `5. Interval`: String,
      `7. Time Zone`: String
  ) derives Codec.AsObject

  final case class IntradayTimeSeriesResponse(
      `Meta Data`: IntradayResponseMetadata
  ) derives Codec.AsObject

  def make[F[_]: {Temporal, Logger}](
      config: AlphaVantageConfig,
      backend: WebSocketStreamBackend[F, Fs2Streams[F]],
      delayBetweenClientFailures: FiniteDuration = 10.seconds
  ): F[AlphaVantageClient[F]] =
    Cache
      .make[F, (CurrencyPair, Interval), MarketTimeSeriesData](3.minutes, 15.seconds)
      .map(cache => LiveAlphaVantageClient(backend, config, delayBetweenClientFailures, cache))
}
