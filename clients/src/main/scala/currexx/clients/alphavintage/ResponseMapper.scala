package currexx.clients.alphavintage

import currexx.domain.errors.AppError
import currexx.domain.market.{MarketTimeSeriesData, PriceRange}
import io.circe.Json

private[alphavintage] object ResponseMapper {

  def mapDailyTimeSeriesData(json: Json): Either[AppError, List[PriceRange]] = ???

  def mapIntradayTimeSeriesData(json: Json): Either[AppError, List[PriceRange]] = ???
}
