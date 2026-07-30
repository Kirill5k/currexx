package currexx.algorithms.operators

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import kirill5k.common.cats.test.IOWordSpec

class ValidatorSpec extends IOWordSpec {

  /** A validator over a lookup table, plus the record of which individuals it actually scored. */
  private def validatorOver(scores: Map[String, Double], shortlistSize: Int = 3) =
    Ref.of[IO, List[String]](Nil).flatMap { scored =>
      Validator
        .shortlisted[IO, String](shortlistSize)(ind => scored.update(_ :+ ind).as(Fitness(scores.getOrElse(ind, 0.0))))
        .map(_ -> scored)
    }

  private def trained(members: (String, Double)*): EvaluatedPopulation[String] =
    members.toVector.map { case (individual, fitness) => (individual, Fitness(fitness)) }

  "Validator.shortlisted" should {

    "rank individuals by what they scored on held-out evidence rather than by training fitness" in {
      // The training order is deliberately the reverse of the validation order, because a validator that quietly
      // preserved the order it was given would pass every other assertion here.
      validatorOver(Map("a" -> 0.1, "b" -> 0.9))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "b" -> 0.1)) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("b", "a")
          validated.map(_._3) mustBe Vector(Fitness(0.9), Fitness(0.1))
        }
    }

    "carry the training fitness through untouched alongside the validation one" in {
      // The pair is what says whether a run found anything; a validator that recomputed or dropped the training figure
      // would leave the retained-fraction reading comparing a number against itself.
      validatorOver(Map("a" -> 0.2))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.8)) }
        .asserting(_ mustBe Vector(("a", Fitness(0.8), Fitness(0.2))))
    }

    "spend the shortlist on distinct individuals rather than on copies of one" in {
      // A converged population is mostly duplicates. Truncating before deduplicating would send the same individual to
      // the held-out evidence three times and call it a shortlist of three.
      validatorOver(Map.empty)
        .flatMap { case (validator, scored) =>
          validator.validate(trained("a" -> 0.9, "a" -> 0.9, "b" -> 0.8, "c" -> 0.7, "d" -> 0.6)) >> scored.get
        }
        .asserting(_ mustBe List("a", "b", "c"))
    }

    "keep the training order among individuals that scored the same on held-out evidence" in {
      // Every candidate of a run that found nothing is tied on zero, and a tie broken arbitrarily would make the
      // reported champion depend on iteration order rather than on anything measured.
      validatorOver(Map.empty)
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "b" -> 0.8, "c" -> 0.7)) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("a", "b", "c")
          validated.map(_._3) mustBe Vector(Fitness(0.0), Fitness(0.0), Fitness(0.0))
        }
    }

    "score an empty population without consulting the held-out evidence" in {
      validatorOver(Map.empty)
        .flatMap { case (validator, scored) => validator.validate(Vector.empty).flatMap(v => scored.get.map(v -> _)) }
        .asserting { case (validated, scored) =>
          validated mustBe empty
          scored mustBe empty
        }
    }
  }

  "Validator.none" should {
    "repeat the training fitness rather than inventing a validation one" in {
      Validator
        .none[IO, String]
        .flatMap(_.validate(trained("a" -> 0.9, "b" -> 0.1)))
        .asserting(_ mustBe Vector(("a", Fitness(0.9), Fitness(0.9)), ("b", Fitness(0.1), Fitness(0.1))))
    }
  }
}
