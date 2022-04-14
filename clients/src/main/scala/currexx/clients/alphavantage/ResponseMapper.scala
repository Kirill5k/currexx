package currexx.clients.alphavantage

import cats.syntax.either.*
import currexx.clients.alphavantage.AlphaVantageClient.{DailyResponseMetadata, IntradayResponseMetadata, OHLC}
import currexx.domain.errors.AppError
import currexx.domain.market.{MarketTimeSeriesData, PriceRange}
import io.circe.JsonObject

import java.time.{LocalDate, LocalDateTime, ZoneId}
import scala.collection.immutable.ListMap

private[alphavantage] object ResponseMapper {

  def mapDailyTimeSeriesData(res: JsonObject): Either[AppError, List[PriceRange]] =
    for
      metaJson   <- res("Meta Data").toRight(AppError.JsonParsingFailure(res.toString, "Missing 'Meta Data' field"))
      meta       <- metaJson.as[DailyResponseMetadata].leftMap(e => AppError.JsonParsingFailure(res.toString, e.getMessage))
      pricesJson <- res("Time Series FX (Daily)").toRight(AppError.JsonParsingFailure(res.toString, "Missing 'Time Series FX (Daily)' field"))
      prices     <- pricesJson.as[ListMap[String, OHLC]].leftMap(e => AppError.JsonParsingFailure(res.toString, e.getMessage))
    yield prices.map { case (date, priceRange) =>
      PriceRange(
        priceRange.`1. open`,
        priceRange.`2. high`,
        priceRange.`3. low`,
        priceRange.`4. close`,
        LocalDate.parse(date).atStartOfDay().atZone(ZoneId.of(meta.`6. Time Zone`)).toInstant
      )
    }.toList

  def mapIntradayTimeSeriesData(res: JsonObject): Either[AppError, List[PriceRange]] =
    for
      metaJson   <- res("Meta Data").toRight(AppError.JsonParsingFailure(res.toString, "Missing 'Meta Data' field"))
      meta       <- metaJson.as[IntradayResponseMetadata].leftMap(e => AppError.JsonParsingFailure(res.toString, e.getMessage))
      timeSeriesField = s"Time Series FX (${meta.`5. Interval`})"
      pricesJson <- res(timeSeriesField).toRight(AppError.JsonParsingFailure(res.toString, s"Missing '$timeSeriesField' field"))
      prices     <- pricesJson.as[ListMap[String, OHLC]].leftMap(e => AppError.JsonParsingFailure(res.toString, e.getMessage))
    yield prices.map { case (date, priceRange) =>
      PriceRange(
        priceRange.`1. open`,
        priceRange.`2. high`,
        priceRange.`3. low`,
        priceRange.`4. close`,
        LocalDateTime.parse(date.replaceFirst(" ", "T")).atZone(ZoneId.of(meta.`7. Time Zone`)).toInstant
      )
    }.toList
}
