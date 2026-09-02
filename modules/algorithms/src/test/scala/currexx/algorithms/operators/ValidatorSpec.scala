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

    "rank individuals by what they scored on held-out evidence rather than by training fitness" in
      // The training order is deliberately the reverse of the validation order, because a validator that quietly
      // preserved the order it was given would pass every other assertion here. With two candidates the two ranks
      // always sum to the same total, so this is the tie-break doing the work: held-out evidence leads.
      validatorOver(Map("a" -> 0.1, "b" -> 0.9))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "b" -> 0.1)) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("b", "a")
          validated.map(_._3) mustBe Vector(Fitness(0.9), Fitness(0.1))
        }

    "carry the training fitness through untouched alongside the validation one" in
      // The pair is what says whether a run found anything; a validator that recomputed or dropped the training figure
      // would leave the retained-fraction reading comparing a number against itself.
      validatorOver(Map("a" -> 0.2))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.8)) }
        .asserting(_ mustBe Vector(("a", Fitness(0.8), Fitness(0.2))))

    "spend the shortlist on distinct individuals rather than on copies of one" in
      // A converged population is mostly duplicates. Truncating before deduplicating would send the same individual to
      // the held-out evidence three times and call it a shortlist of three.
      validatorOver(Map.empty)
        .flatMap { case (validator, scored) =>
          validator.validate(trained("a" -> 0.9, "a" -> 0.9, "b" -> 0.8, "c" -> 0.7, "d" -> 0.6)) >> scored.get
        }
        .asserting(_ mustBe List("a", "b", "c"))

    "keep the training order among individuals that scored the same on held-out evidence" in
      // Every candidate of a run that found nothing is tied on zero, and a tie broken arbitrarily would make the
      // reported champion depend on iteration order rather than on anything measured.
      validatorOver(Map.empty)
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "b" -> 0.8, "c" -> 0.7)) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("a", "b", "c")
          validated.map(_._3) mustBe Vector(Fitness(0.0), Fitness(0.0), Fitness(0.0))
        }

    "break a near-tie on held-out evidence with the training rank rather than on the last decimal" in
      // The change of 2026-09-02. `b` wins the held-out score by a hair and loses on training by a mile; a plain argmax
      // of the validation figure takes `b`, which over eleven real rounds meant choosing between candidates separated
      // by a tenth of a percent. Inside the band the two count as tied out of sample, so training decides and `a` wins.
      validatorOver(Map("a" -> 0.50, "b" -> 0.51, "c" -> 0.10))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "c" -> 0.5, "b" -> 0.1)) }
        .asserting(_.map(_._1) mustBe Vector("a", "b", "c"))

    "never rank a candidate that earned nothing out of sample above one that did" in
      // The gate is unchanged and comes first: no training figure buys a way past an empty held-out result.
      validatorOver(Map("a" -> 0.0, "b" -> 0.2))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.99, "b" -> 0.01)) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("b", "a")
          validated.map(_._3) mustBe Vector(Fitness(0.2), Fitness(0.0))
        }

    "keep the training order among the candidates that earned nothing out of sample" in
      // A run that found nothing still has to hand back its training-ranked best, so that it reports itself as having
      // selected nothing rather than picking whichever tie the sort happened to leave on top.
      validatorOver(Map("a" -> 0.0, "b" -> 0.3, "c" -> 0.0))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.9, "b" -> 0.8, "c" -> 0.7)) }
        .asserting(_.map(_._1) mustBe Vector("b", "a", "c"))

    "leave a held-out result that is clearly ahead alone, however good the training figure behind it" in
      // The guard on the tie-break. A rank-sum over both scores was tried first and rejected here: ranks discard
      // magnitude, so replaying one over the rounds of 2026-09-01/02 swapped a validation of 0.1048 for 0.0045 to gain
      // 0.02 of training. A gap this size is evidence, not noise, and no training figure may buy past it.
      validatorOver(Map("a" -> 0.10, "b" -> 0.90))
        .flatMap { case (validator, _) => validator.validate(trained("a" -> 0.99, "b" -> 0.01)) }
        .asserting(_.map(_._1) mustBe Vector("b", "a"))

    "score an empty population without consulting the held-out evidence" in
      validatorOver(Map.empty)
        .flatMap { case (validator, scored) => validator.validate(Vector.empty).flatMap(v => scored.get.map(v -> _)) }
        .asserting { case (validated, scored) =>
          validated mustBe empty
          scored mustBe empty
        }
  }

  "Validator.TieBand" should {
    "decide which candidates count as tied, and say so in the words a report uses" in {
      val band = Validator.TieBand(0.05)
      band.describe mustBe "5%"
      band.ties(best = 0.51, candidate = 0.50) mustBe true
      band.ties(best = 0.51, candidate = 0.48) mustBe false
    }

    "narrow the tie-break when a caller asks for a tighter band" in {
      // The band is the one empirical number in this file, so it is passed in and a test can move it. At 5% the pair
      // below is tied and training decides; at 1% it is not and the held-out score stands.
      val scores  = Map("a" -> 0.50, "b" -> 0.51)
      val runWith = (band: Validator.TieBand) =>
        Ref
          .of[IO, List[String]](Nil)
          .flatMap { scored =>
            Validator
              .shortlisted[IO, String](3, band)(ind => scored.update(_ :+ ind).as(Fitness(scores.getOrElse(ind, 0.0))))
              .flatMap(_.validate(trained("a" -> 0.9, "b" -> 0.1)))
          }
          .map(_.map(_._1))

      runWith(Validator.TieBand(0.05))
        .flatMap(wide => runWith(Validator.TieBand(0.01)).map(wide -> _))
        .asserting { case (wide, narrow) =>
          wide mustBe Vector("a", "b")
          narrow mustBe Vector("b", "a")
        }
    }
  }

  "Validator.none" should {
    "repeat the training fitness rather than inventing a validation one" in
      Validator
        .none[IO, String]
        .flatMap(_.validate(trained("a" -> 0.9, "b" -> 0.1)))
        .asserting(_ mustBe Vector(("a", Fitness(0.9), Fitness(0.9)), ("b", Fitness(0.1), Fitness(0.1))))
  }
}
