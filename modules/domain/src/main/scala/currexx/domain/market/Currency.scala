package currexx.domain.market

import io.circe.{Decoder, Encoder}

opaque type Currency = String

object Currency {
  val CAD: Currency = "CAD"
  val EUR: Currency = "EUR"
  val GBP: Currency = "GBP"
  val USD: Currency = "USD"

  val defaultSet: Set[String] = Set("AUD", "CAD", "CHF", "DKK", "EUR", "GBP", "JPY", "NOK", "NZD", "PLN", "RUB", "USD")

  def from(code: String): Either[String, Currency] = Either.cond(
    defaultSet.contains(code),
    code,
    s"Unknown currency code $code; Available currencies are: ${defaultSet.mkString(", ")}"
  )

  extension (currency: Currency) def code: String = currency

  inline given Decoder[Currency] = Decoder.decodeString.emap[Currency](Currency.from)
  inline given Encoder[Currency] = Encoder.encodeString.contramap(_.code)
}
