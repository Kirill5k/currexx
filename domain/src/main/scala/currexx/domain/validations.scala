package currexx.domain

import eu.timepit.refined.api.{Refined, RefinedTypeOps, Validate}
import eu.timepit.refined.string.MatchesRegex
import org.bson.types.ObjectId
import squants.market.defaultMoneyContext

object validations {
  type EmailString = String Refined MatchesRegex["^[a-zA-Z0-9.]+@[a-zA-Z0-9]+\\.[a-zA-Z]+$"]

  type IdString = String Refined ValidId
  object ValidIdString extends RefinedTypeOps[IdString, String]

  final case class ValidId()
  inline given Validate.Plain[String, ValidId] =
    Validate.fromPredicate(id => ObjectId.isValid(id), id => s"($id is valid id)", ValidId())

  type CurrencyPairString = String Refined ValidCurrencyPair
  object ValidCurrencyPairString extends RefinedTypeOps[CurrencyPairString, String]

  final case class ValidCurrencyPair()
  inline given Validate[String, ValidCurrencyPair] =
    Validate.fromPredicate(
      cp => cp.matches("^[A-Z]{3}\\/[A-Z]{3}$") && cp.split("\\/").forall(defaultMoneyContext.currencyMap.keySet.contains),
      cp => s"($cp is valid currency pair)",
      ValidCurrencyPair()
    )
}
