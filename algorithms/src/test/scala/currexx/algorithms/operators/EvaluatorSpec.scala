package currexx.algorithms.operators

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
  }

  def evaluate(attempts: Ref[IO, Int])(individual: String): IO[(String, Fitness)] =
    attempts.get.flatMap {
      case 0 => IO.pure((individual, Fitness(BigDecimal(1)))).flatTap(_ => attempts.update(_ + 1))
      case _ => IO.raiseError(new RuntimeException("should not happen"))
    }
}
