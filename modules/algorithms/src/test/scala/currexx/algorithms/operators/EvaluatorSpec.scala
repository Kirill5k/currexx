package currexx.algorithms.operators

import cats.effect.{IO, Ref}
import cats.syntax.traverse.*
import currexx.algorithms.{EvaluationPhase, Fitness}
import kirill5k.common.cats.test.IOWordSpec

class EvaluatorSpec extends IOWordSpec {

  "Evaluator.cached" should {
    List(
      ("store evaluation results in cache", List(EvaluationPhase.Search(0), EvaluationPhase.Search(1))),
      ("answer every phase from the one score it took", List(EvaluationPhase.Search(3), EvaluationPhase.Rescore))
    ).foreach { case (description, phases) =>
      description in {
        // Each sequence starts with a fresh cache. The objective raises on a second call, including across phases.
        val result = for
          attempts  <- Ref.of[IO, Int](0)
          evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
          results   <- phases.traverse(evaluator.evaluateIndividual("foo", _))
        yield results

        result.asserting(_ mustBe List.fill(phases.size)("foo" -> Fitness(1.0)))
      }
    }

    "evaluate each unique individual only once under concurrent access" in {
      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
        phase = EvaluationPhase.Search(0)
        (r1, r2) <- IO.both(evaluator.evaluateIndividual("foo", phase), evaluator.evaluateIndividual("foo", phase))
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
