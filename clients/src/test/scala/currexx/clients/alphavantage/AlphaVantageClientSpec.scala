package currexx.clients.alphavantage

import currexx.clients.ApiClientSpec
import currexx.domain.market.CurrencyPair
import squants.market.{GBP, USD}

class AlphaVantageClientSpec extends ApiClientSpec {
  
  val pair = CurrencyPair(GBP, USD)
  
  "An AlphaVantageClient" should {
    "retrieve hourly price time series data" in {
      pending
    }

    "retrieve daily price time series data" in {
      pending
    }
  }
}
