package currexx.backtest

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StrategyCatalogueSpec extends AnyWordSpec with Matchers {
  "StrategyCatalogue" should {
    "preserve batch membership, strategy identities and evaluation order" in {
      val expected = List(
        "s2_optimized"       -> TestStrategy.s2_optimized,
        "s2_optimized_v2"    -> TestStrategy.s2_optimized_v2,
        "s10"                -> TestStrategy.s10,
        "s10_optimized"      -> TestStrategy.s10_optimized,
        "s10_v2"             -> TestStrategy.s10_v2,
        "s5_optimized_v2"    -> TestStrategy.s5_optimized_v2,
        "s5_optimized_v3"    -> TestStrategy.s5_optimized_v3,
        "s1_v2_optimized"    -> TestStrategy.s1_v2_optimized,
        "s1_v2_optimized_v4" -> TestStrategy.s1_v2_optimized_v4,
        "s4_optimized_v1"    -> TestStrategy.s4_optimized_v1,
        "s4_optimized_v2"    -> TestStrategy.s4_optimized_v2,
        "s6_optimized"       -> TestStrategy.s6_optimized,
        "s6"                 -> TestStrategy.s6,
        "s13"                -> TestStrategy.s13,
        "s13_optimized"      -> TestStrategy.s13_optimized
      )

      StrategyCatalogue.batch mustBe expected
      BatchBacktester.strategies mustBe expected
    }

    "retain named lineage strategies without including them in batch runs" in {
      StrategyCatalogue.entries must have size 17
      StrategyCatalogue.entries.map(_.name).distinct must have size 17
      StrategyCatalogue.entries.filterNot(_.includeInBatch) mustBe List(
        StrategyCatalogue.Entry("s12", TestStrategy.s12, includeInBatch = false),
        StrategyCatalogue.Entry("s12_optimized", TestStrategy.s12_optimized, includeInBatch = false)
      )
    }
  }
}
