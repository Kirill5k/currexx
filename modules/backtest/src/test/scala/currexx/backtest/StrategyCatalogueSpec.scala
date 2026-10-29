package currexx.backtest

import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StrategyCatalogueSpec extends AnyWordSpec with Matchers {
  "StrategyCatalogue" should {
    "register every public strategy definition under its own name" in {
      // Discover definitions only in the test: production registration keeps deliberate batch inclusion and ordering.
      val definitions = TestStrategy.getClass.getMethods.iterator
        .filter(method => method.getParameterCount == 0 && method.getReturnType == classOf[TestStrategy])
        .map(method => method.getName -> method.invoke(TestStrategy).asInstanceOf[TestStrategy])
        .toMap
      val names = StrategyCatalogue.entries.map(_.name)

      withClue("Duplicate catalogue names: ") {
        names.diff(names.distinct) mustBe empty
      }
      withClue("Unregistered TestStrategy definitions: ") {
        definitions.keySet -- names.toSet mustBe empty
      }
      withClue("Catalogue entries without a matching TestStrategy definition: ") {
        names.toSet -- definitions.keySet mustBe empty
      }
      StrategyCatalogue.entries.foreach { entry =>
        withClue(s"Catalogue entry '${entry.name}' refers to the wrong definition: ") {
          (entry.strategy eq definitions(entry.name)) mustBe true
        }
      }
    }

    "preserve batch membership, strategy identities and evaluation order" in {
      val expected = List(
        "s2_optimized"       -> TestStrategy.s2_optimized,
        "s2_optimized_v2"    -> TestStrategy.s2_optimized_v2,
        "s2_optimized_v3"    -> TestStrategy.s2_optimized_v3,
        "s10"                -> TestStrategy.s10,
        "s10_optimized"      -> TestStrategy.s10_optimized,
        "s10_v2"             -> TestStrategy.s10_v2,
        "s5_optimized_v2"    -> TestStrategy.s5_optimized_v2,
        "s5_optimized_v3"    -> TestStrategy.s5_optimized_v3,
        "s1_v2_optimized"    -> TestStrategy.s1_v2_optimized,
        "s1_v2_optimized_v2" -> TestStrategy.s1_v2_optimized_v2,
        "s4_optimized_v1"    -> TestStrategy.s4_optimized_v1,
        "s4_optimized_v2"    -> TestStrategy.s4_optimized_v2,
        "s6_optimized"       -> TestStrategy.s6_optimized,
        "s6"                 -> TestStrategy.s6,
        "s13"                -> TestStrategy.s13,
        "s13_optimized"      -> TestStrategy.s13_optimized,
        "s12"                -> TestStrategy.s12,
        "s12_optimized"      -> TestStrategy.s12_optimized
      )

      StrategyCatalogue.batch mustBe expected
      BatchBacktester.strategies mustBe expected
    }

    "include every retained strategy in batch evaluation" in {
      StrategyCatalogue.entries.filterNot(_.includeInBatch) mustBe empty
    }
  }
}
