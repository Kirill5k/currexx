package currexx.algorithms.operators

import cats.effect.{IO, Ref}
import currexx.algorithms.Fitness
import kirill5k.common.cats.test.IOWordSpec

class EvaluatorSpec extends IOWordSpec {

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
        fitness mustBe Fitness(1.0)
      }
    }

    "evaluate each unique individual only once under concurrent access" in {
      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
        (r1, r2)  <- IO.both(evaluator.evaluateIndividual("foo"), evaluator.evaluateIndividual("foo"))
      yield (r1, r2)

      result.asserting { (r1, r2) =>
        r1._2 mustBe Fitness(1.0)
        r2._2 mustBe Fitness(1.0)
      }
    }
  }

  def evaluate[A](attempts: Ref[IO, Int])(individual: A): IO[(A, Fitness)] =
    attempts.get.flatMap {
      case 0 => IO.pure((individual, Fitness(1.0))).flatTap(_ => attempts.update(_ + 1))
      case _ => IO.raiseError(new RuntimeException("should not happen"))
    }
}
