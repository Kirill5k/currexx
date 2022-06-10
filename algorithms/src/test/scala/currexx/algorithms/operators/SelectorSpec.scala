package currexx.algorithms.operators

import currexx.algorithms.Fitness
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import scala.util.Random

class SelectorSpec extends AnyWordSpec with Matchers {

  "A Selector.RouletteWheel" should {

    "sort population by fittest candidates based on probability and distribute them in pairs" in {
      val population = Vector(
        (1, Fitness(4.0)), // 6
        (2, Fitness(2.0)),
        (3, Fitness(5.0)), // 4
        (4, Fitness(10.0)), // 5
        (5, Fitness(1.0)),
        (6, Fitness(20.0)), // 2
        (7, Fitness(35.0)), // 1
        (8, Fitness(7.0)), // 3
        (9, Fitness(-7.0)),
        (10, Fitness(-1.0))
      )

      val selector = Selector.rouletteWheel[Int]

      given r: Random   = Random(42)
      val newPopulation = selector.selectPairs(population, 6)

      newPopulation mustBe Vector((7,6), (8,3), (4,1))
    }
  }
}