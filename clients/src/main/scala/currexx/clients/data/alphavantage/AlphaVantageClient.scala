package currexx.clients.data.alphavantage

import cats.data.NonEmptyList
import cats.effect.Temporal
import cats.syntax.apply.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.clients.data.MarketDataClient
import currexx.clients.{ClientConfig}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}
import io.circe.{Codec, JsonObject}
import org.typelevel.log4cats.Logger
import sttp.client3.*
import sttp.client3.circe.*
import sttp.model.{StatusCode, Uri}

import scala.concurrent.duration.*

final private[clients] class AlphaVantageClient[F[_]](
    private val config: ClientConfig,
    override protected val backend: SttpBackend[F, Any]
)(using
    F: Temporal[F],
    logger: Logger[F]
) extends MarketDataClient[F]:
  import AlphaVantageClient.*

  override protected val name: String                         = "alpha-vantage"
  override protected val delayBetweenFailures: FiniteDuration = 5.seconds

  override def timeSeriesData(pair: CurrencyPair, interval: Interval): F[MarketTimeSeriesData] =
    interval match
      case Interval.D1                                                           => dailyTimeSeriesData(pair)
      case Interval.M1 | Interval.M5 | Interval.M15 | Interval.M30 | Interval.H1 => intradayTimeSeriesData(pair, interval)

  private def dailyTimeSeriesData(pair: CurrencyPair): F[MarketTimeSeriesData] = {
    val params = Map(
      "function"    -> "FX_DAILY",
      "from_symbol" -> pair.base.code,
      "to_symbol"   -> pair.quote.code,
      "apikey"      -> config.apiKey.get
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
      "interval" -> (interval match
        case Interval.M1  => "1min"
        case Interval.M5  => "5min"
        case Interval.M15 => "15min"
        case Interval.M30 => "30min"
        case Interval.H1  => "60min"
        case _            => ""
      )
    )
    sendRequest(uri"${config.baseUri}/query?$params", ResponseMapper.mapIntradayTimeSeriesData)
      .map(prs => MarketTimeSeriesData(pair, interval, prs))
  }

  private def sendRequest(uri: Uri, mapper: JsonObject => Either[AppError, NonEmptyList[PriceRange]]): F[NonEmptyList[PriceRange]] =
    dispatch(basicRequest.get(uri).response(asJson[JsonObject]))
      .flatMap { r =>
        r.body match {
          case Right(value) =>
            F.fromEither(mapper(value))
          case Left(HttpError(responseBody, StatusCode.Forbidden)) =>
            logger.error(s"$name-client/forbidden\n$responseBody") *>
              F.raiseError(AppError.AccessDenied(s"$name authentication has expired"))
          case Left(HttpError(responseBody, status)) =>
            logger.error(s"$name-client/${status.code}\n$responseBody") *>
              F.sleep(delayBetweenFailures) *> sendRequest(uri, mapper)
          case Left(error) =>
            logger.error(s"$name-client/error\n$error") *>
              F.sleep(delayBetweenFailures) *> sendRequest(uri, mapper)
        }
      }

private[clients] object AlphaVantageClient {

  final case class OHLC(
      `1. open`: BigDecimal,
      `2. high`: BigDecimal,
      `3. low`: BigDecimal,
      `4. close`: BigDecimal
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

  def make[F[_]: Temporal: Logger](
      config: ClientConfig,
      backend: SttpBackend[F, Any]
  ): F[MarketDataClient[F]] =
    Temporal[F]
      .raiseWhen(config.apiKey.isEmpty)(new RuntimeException("Cannot create alpha-vantage client without providing api-key"))
      .as(AlphaVantageClient(config, backend))
}
