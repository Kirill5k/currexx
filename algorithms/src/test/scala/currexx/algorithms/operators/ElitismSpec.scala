package currexx.algorithms.operators

import cats.effect.IO
import currexx.algorithms.{Fitness, IOWordSpec}

class ElitismSpec extends IOWordSpec {

  "Elitism.simple" should {
    "select proportion of best individuals from the evaluated and sorted population" in {
      val population = Vector(
        (6, Fitness(20.0)),
        (4, Fitness(10.0)),
        (3, Fitness(5.0)),
        (1, Fitness(4.0)),
        (2, Fitness(2.0)),
        (5, Fitness(1.0))
      )

      val result = Elitism.simple[IO, Int].flatMap(_.select(population, 2))

      result.asserting(_ mustBe List(6, 4))
    }
  }
}
