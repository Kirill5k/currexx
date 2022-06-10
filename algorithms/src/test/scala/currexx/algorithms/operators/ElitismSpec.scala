package currexx.algorithms.operators

import currexx.algorithms.Fitness
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ElitismSpec extends AnyWordSpec with Matchers {

  "An Elitism operator" should {

    "select proportion of fittest individuals" in {
      val population = Vector(
        (1, Fitness(4.0)),
        (2, Fitness(2.0)),
        (3, Fitness(5.0)),
        (4, Fitness(10.0)),
        (5, Fitness(1.0)),
        (6, Fitness(20.0))
      )

      val result = Elitism.simple[Int].select(population, 2)

      result mustBe List(6, 4)
    }
  }
}

