package currexx.algorithms.operators

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.util.Random

class InitialiserSpec extends AsyncWordSpec with Matchers {

  "A Simple initialiser" should {
    val rand                                   = Random(42)
    val stringRandomiser: String => IO[String] = s => IO.delay(rand.shuffle(s.toList).mkString(""))
    val individual                             = "ABCDEFG"

    "initialise population of requested size with identical individuals when shuffle=false" in {
      val result = for
        initialiser <- Initialiser.simple[IO, String](stringRandomiser)
        population  <- initialiser.initialisePopulation(individual, 10, false)
      yield population

      result.unsafeToFuture().map { pop =>
        pop must have size 10
        pop.toSet mustBe Set(individual)
      }
    }

    "initialise population of requested size with randomised individuals when shuffle=true" in {
      val result = for
        initialiser <- Initialiser.simple[IO, String](stringRandomiser)
        population  <- initialiser.initialisePopulation(individual, 10, true)
      yield population

      result.unsafeToFuture().map { pop =>
        pop must have size 10
        pop.toSet must not be Set(individual)
      }
    }
  }
}
