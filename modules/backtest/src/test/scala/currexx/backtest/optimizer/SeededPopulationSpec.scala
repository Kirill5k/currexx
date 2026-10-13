package currexx.backtest.optimizer

import cats.effect.{IO, Ref}
import cats.syntax.traverse.*
import kirill5k.common.cats.test.IOWordSpec

class SeededPopulationSpec extends IOWordSpec {
  "SeededPopulation" should {
    "allocate ordered clones, nearby radii and immigrants in both population mixes" in {
      val seeds = Vector("a", "b", "a")
      val refining = (
        false,
        Vector("a", "b", "a", "b", "a", "b", "a", "b", "a"),
        Vector.fill(9)(Vector("a@1.0", "b@1.0")).flatten ++ Vector.fill(4)("immigrant")
      )
      val exploring = (
        true,
        Vector("a", "b", "a", "b"),
        Vector.fill(3)(Vector("a@1.0", "b@3.0", "a@6.0", "b@1.0", "a@3.0", "b@6.0")).flatten.take(17) ++
          Vector.fill(10)("immigrant")
      )

      List(refining, exploring).traverse { (shuffle, clones, generated) =>
        for
          calls <- Ref.of[IO, Vector[String]](Vector.empty)
          record = (value: String) => calls.update(_ :+ value).as(value)
          population <- SeededPopulation.build[IO, String](seeds, 31, shuffle)(
            (seed, radius) => record(s"$seed@$radius"),
            () => record("immigrant")
          )
          executed <- calls.get
        yield (population, executed, clones ++ generated, generated)
      }.asserting { results =>
        results.foreach { (actual, executed, expected, generated) =>
          actual mustBe expected
          executed mustBe generated
        }
        succeed
      }
    }

    "retain every distinct seed in a minimal population without invoking generators" in {
      List(false, true).traverse { shuffle =>
        SeededPopulation.build[IO, String](Vector("b", "a", "b"), 2, shuffle)(
          (_, _) => fail("A minimal population needs no neighbours"),
          () => fail("A minimal population needs no immigrants")
        )
      }.asserting { populations =>
        populations.foreach(_ mustBe Vector("b", "a"))
        succeed
      }
    }

    "reject empty seeds and insufficient budgets as failed effects before invoking generators" in {
      val invalid = List(Vector.empty[String] -> 1, Vector("a") -> 0, Vector("a") -> -1, Vector("a", "b") -> 1)
      invalid.traverse { (seeds, size) =>
        SeededPopulation.build[IO, String](seeds, size, shuffle = true)(
          (_, _) => fail("Invalid populations must not generate neighbours"),
          () => fail("Invalid populations must not generate immigrants")
        ).attempt
      }.asserting { results =>
        results.foreach(_.left.map(_.getMessage) mustBe Left("Population size must be positive and hold all distinct seeds"))
        succeed
      }
    }
  }
}
