package currexx.algorithms.operators

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.algorithms.{CatsSpec, Fitness}

class ElitismSpec extends CatsSpec {

  "Elitism.simple" should {

    "select proportion of fittest individuals" in {
      val population = Vector(
        (1, Fitness(4.0)),
        (2, Fitness(2.0)),
        (3, Fitness(5.0)),
        (4, Fitness(10.0)),
        (5, Fitness(1.0)),
        (6, Fitness(20.0))
      )

      val result = Elitism.simple[IO, Int].flatMap(_.select(population, 2))

      result.asserting(_ mustBe List(6, 4))
    }
  }
}

