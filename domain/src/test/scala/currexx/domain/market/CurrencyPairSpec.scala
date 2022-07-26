package currexx.domain.market

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import squants.market.{EUR, USD}

class CurrencyPairSpec extends AnyWordSpec with Matchers {

  "CurrencyPair" should {
    "be created from a string with slash" in {
      CurrencyPair.from("EUR/USD") mustBe Right(CurrencyPair(EUR, USD))
    }

    "be created from a string without slash" in {
      CurrencyPair.from("EURUSD") mustBe Right(CurrencyPair(EUR, USD))
    }
  }
}
