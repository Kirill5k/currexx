package currexx.algorithms.operators

import cats.Show
import cats.effect.{IO, Ref}
import currexx.algorithms.{CatsSpec, Fitness}

class EvaluatorSpec extends CatsSpec {

  "Evaluator.cached" should {
    "store evaluation results in cache" in {
      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
        _         <- evaluator.evaluateIndividual("foo")
        res       <- evaluator.evaluateIndividual("foo")
      yield res

      result.asserting { (ind, fitness) =>
        ind mustBe "foo"
        fitness mustBe Fitness(BigDecimal(1))
      }
    }

    "work with arrays" in {
      given showArray[A](using S: Show[A]): Show[Array[A]] = new Show[Array[A]]:
        override def show(t: Array[A]): String = t.map(S.show).mkString(",")

      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, Array[Int]](evaluate(attempts))
        _         <- evaluator.evaluateIndividual(Array(0, 1, 2, 3))
        res       <- evaluator.evaluateIndividual(Array(0, 1, 2, 3))
      yield res

      result.asserting { (ind, fitness) =>
        ind mustBe Array(0, 1, 2, 3)
        fitness mustBe Fitness(BigDecimal(1))
      }
    }
  }

  def evaluate[A](attempts: Ref[IO, Int])(individual: A): IO[(A, Fitness)] =
    attempts.get.flatMap {
      case 0 => IO.pure((individual, Fitness(BigDecimal(1)))).flatTap(_ => attempts.update(_ + 1))
      case _ => IO.raiseError(new RuntimeException("should not happen"))
    }
}
