package currexx.core.common.http

import cats.syntax.option.*
import currexx.domain.market.Trend
import eu.timepit.refined.types.string.NonEmptyString
import squants.Money
import squants.market.Currency
import currexx.domain.validations.{EmailString, IdString}
import sttp.tapir.generic.SchemaDerivation
import sttp.tapir.{FieldName, Schema}
import sttp.tapir.Schema.SName
import sttp.tapir.SchemaType.{SProduct, SProductField}

transparent trait TapirSchema extends SchemaDerivation {
  inline given Schema[IdString]       = Schema.string
  inline given Schema[NonEmptyString] = Schema.string
  inline given Schema[EmailString]    = Schema.string
  inline given Schema[Currency]       = Schema.string
  inline given Schema[Trend]          = Schema.string

  inline given (using currencySchema: Schema[Currency]): Schema[Money] = Schema(
    SProduct(
      List(
        SProductField(FieldName("currency"), currencySchema, _.currency.some),
        SProductField(FieldName("amount"), Schema.schemaForDouble, _.value.some)
      )
    ),
    Some(SName("Money"))
  )
}
