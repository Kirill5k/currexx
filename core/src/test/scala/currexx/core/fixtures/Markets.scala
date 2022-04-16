package currexx.core.fixtures

import squants.market.{EUR, GBP, USD}
import currexx.domain.market.{CurrencyPair, Interval, MarketTimeSeriesData, PriceRange}

import java.time.Instant

object Markets {
  lazy val gbpeur = CurrencyPair(GBP, EUR)
  lazy val gbpusd = CurrencyPair(GBP, USD)

  lazy val ts             = Instant.now
  lazy val priceRange     = PriceRange(BigDecimal(2.0), BigDecimal(4.0), BigDecimal(1.0), BigDecimal(3.0), ts)
  lazy val timeSeriesData = MarketTimeSeriesData(gbpeur, Interval.H1, List(priceRange))
}
