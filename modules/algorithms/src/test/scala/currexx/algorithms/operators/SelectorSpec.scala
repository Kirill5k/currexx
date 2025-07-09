package currexx.algorithms.operators

import cats.effect.IO
import kirill5k.common.cats.test.IOWordSpec
import currexx.algorithms.Fitness

import scala.util.Random

class SelectorSpec extends IOWordSpec {

  "Selector.rouletteWheel" should {

    "distribute evaluated population into pairs" in {
      val population = Vector(
        (7, Fitness(35.0)), // 1
        (6, Fitness(20.0)), // 2
        (4, Fitness(10.0)), // 5
        (8, Fitness(7.0)),  // 3
        (3, Fitness(5.0)),  // 4
        (1, Fitness(4.0)),  // 6
        (2, Fitness(2.0)),
        (5, Fitness(1.0)),
        (10, Fitness(-1.0)),
        (9, Fitness(-7.0))
      )

      given Random = Random(42)
      val result   = Selector.rouletteWheel[IO, Int].flatMap(_.selectPairs(population, 6))

      result.asserting(_ mustBe Vector((4, 6), (7, 7), (6, 8)))
    }
  }

  "Selector.tournament" should {

    "distribute evaluated population into pairs" in {
      val population = Vector(
        (7, Fitness(35.0)), // 1
        (6, Fitness(20.0)), // 2
        (4, Fitness(10.0)), // 5
        (8, Fitness(7.0)), // 3
        (3, Fitness(5.0)), // 4
        (1, Fitness(4.0)), // 6
        (2, Fitness(2.0)),
        (5, Fitness(1.0)),
        (10, Fitness(-1.0)),
        (9, Fitness(-7.0))
      )

      given Random = Random(42)

      val result = Selector.tournament[IO, Int].flatMap(_.selectPairs(population, 6))

      result.asserting(_ mustBe Vector((7, 10), (4, 6), (1, 2)))
    }
  }
}
