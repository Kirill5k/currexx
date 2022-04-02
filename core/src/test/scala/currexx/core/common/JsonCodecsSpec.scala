package currexx.core.common

import currexx.core.auth.session.SessionId
import io.circe.DecodingFailure
import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.wordspec.AnyWordSpec
import org.scalatest.matchers.must.Matchers
import squants.Money
import squants.market.{Currency, GBP}

class JsonCodecsSpec extends AnyWordSpec with Matchers with JsonCodecs {

  "Currency codec" should {
    "convert json to currency" in {
      val currency = """"GBP""""
      decode[Currency](currency) mustBe Right(GBP)
    }

    "convert money to json" in {
      GBP.asInstanceOf[Currency].asJson.noSpaces mustBe """"GBP""""
    }

    "return error on unrecognized currency code" in {
      val currency = """"FOO""""
      decode[Currency](currency) mustBe Left(DecodingFailure("Code FOO cannot be matched against any context defined Currency. Available Currencies are CAD, CZK, GBP, MXN, CHF, CNY, RUB, NZD, HKD, AUD, SEK, TRY, BRL, KRW, ETH, CLP, INR, LTC, BTC, DKK, XAU, XAG, JPY, ARS, MYR, USD, NOK, NAD, EUR, ZAR", Nil))
    }
  }

  "Money codec" should {
    "convert json to money" in {
      val money = """{"currency":"GBP","amount":1}"""
      decode[Money](money) mustBe Right(GBP(BigDecimal(1.00)))
    }

    "convert money to json" in {
      GBP(BigDecimal(1)).asJson.noSpaces mustBe """{"amount":1.00,"currency":"GBP"}"""
    }
  }

  "Id codec" should {
    "decode and encode ids" in {
      val id = SessionId("FOO")
      val json = id.asJson.noSpaces

      json mustBe """"FOO""""
      decode[SessionId](json) mustBe Right(id)
    }
  }
}

