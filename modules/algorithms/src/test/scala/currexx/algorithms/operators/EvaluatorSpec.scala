package currexx.algorithms.operators

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluationPhase, Fitness}
import kirill5k.common.cats.test.IOWordSpec

class EvaluatorSpec extends IOWordSpec {

  "Evaluator.cached" should {
    "store evaluation results in cache" in {
      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
        _         <- evaluator.evaluateIndividual("foo", EvaluationPhase.Search(0))
        res       <- evaluator.evaluateIndividual("foo", EvaluationPhase.Search(1))
      yield res

      result.asserting { (ind, fitness) =>
        ind mustBe "foo"
        fitness mustBe Fitness(1.0)
      }
    }

    "answer every phase from the one score it took" in {
      // The soundness condition, asserted rather than assumed: `objectiveFn` is handed the individual and nothing else, so it cannot vary
      // with the phase and the cached answer is the answer under all of them. `evaluate` raises on a second call, so a cache that keyed on
      // the phase - the obvious way to make a phase-dependent objective correct, and the wrong thing to do here - would fail this.
      val result = for
        attempts  <- Ref.of[IO, Int](0)
        evaluator <- Evaluator.cached[IO, String](evaluate(attempts))
        searched  <- evaluator.evaluateIndividual("foo", EvaluationPhase.Search(3))
        rescored  <- evaluator.evaluateIndividual("foo", EvaluationPhase.Rescore)
      yield (searched, rescored)

      result.asserting { (searched, rescored) =>
        searched mustBe rescored
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
