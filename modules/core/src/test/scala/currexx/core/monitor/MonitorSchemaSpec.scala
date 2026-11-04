package currexx.core.monitor

import currexx.core.common.http.TapirSchema
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sttp.tapir.{Schema, SchemaType}

class MonitorSchemaSpec extends AnyWordSpec with Matchers with TapirSchema {
  "Market-data monitor schemas" should {
    "describe candleCount as an optional integer with the supported bounds" in {
      val schemas: List[Schema[?]] = List(
        Schema.derived[MonitorController.CreateMonitorRequest.MarketData],
        Schema.derived[MonitorController.MonitorView.MarketData]
      )

      schemas.foreach { schema =>
        schema.schemaType match
          case SchemaType.SProduct(fields) =>
            val count = fields.find(_.name.name == "candleCount").get.schema
            count.isOptional mustBe true
            count.schemaType match
              case SchemaType.SOption(element) =>
                element.schemaType.show mustBe "integer"
                element.showValidators mustBe Some("all(>=1,<=4999)")
              case other => fail(s"Expected an optional candle count, got $other")
          case other => fail(s"Expected a monitor object, got $other")
      }
    }
  }
}
