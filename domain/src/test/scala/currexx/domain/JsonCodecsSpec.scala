package currexx.domain

import currexx.domain.session.SessionId
import currexx.domain.market.{Currency, Interval}
import io.circe.{DecodingFailure, Error}
import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class JsonCodecsSpec extends AnyWordSpec with Matchers with JsonCodecs {

  "Interval codec" should {
    "decode and encode interval" in {
      val interval = Interval.H1
      val json     = interval.asJson.noSpaces

      json mustBe """"H1""""
      decode[Interval](json) mustBe Right(Interval.H1)
    }
  }

  "Currency codec" should {
    "convert currency to json" in {
      Currency.GBP.asJson.noSpaces mustBe """"GBP""""
    }

    "return error on unrecognized currency code" in {
      val currency: String = """"FOO""""
      decode[Currency](currency) mustBe Left(
        DecodingFailure(
          "Unknown currency code FOO; Available currencies are: PLN, CAD, AUD, GBP, CHF, DKK, JPY, USD, RUB, NZD, NOK, EUR",
          Nil
        )
      )
    }

    "convert json to currency" in {
      val currency = """"GBP""""
      decode[Currency](currency) mustBe Right(Currency.GBP)
    }
  }

  "Id codec" should {
    "decode and encode ids" in {
      val id   = SessionId("FOO")
      val json = id.asJson.noSpaces

      json mustBe """"FOO""""
      decode[SessionId](json) mustBe Right(id)
    }
  }
}
