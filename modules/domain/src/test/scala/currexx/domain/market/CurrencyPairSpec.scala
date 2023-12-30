package currexx.domain.market

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import currexx.domain.market.Currency.{EUR, USD}

class CurrencyPairSpec extends AnyWordSpec with Matchers {

  "CurrencyPair" should {
    "be created from a string with slash" in {
      CurrencyPair.from("EUR/USD") mustBe Right(CurrencyPair(EUR, USD))
    }

    "be created from a string without slash" in {
      CurrencyPair.from("EURUSD") mustBe Right(CurrencyPair(EUR, USD))
    }
  }

  "CurrencyPair codec" should {
    "convert json to currency pair" in {
      val result = decode[CurrencyPair](""""EURUSD"""")

      result mustBe Right(CurrencyPair(EUR, USD))
    }

    "convert currency to json" in {
      CurrencyPair(EUR, USD).asJson.noSpaces mustBe """"EURUSD""""
    }
  }
}
