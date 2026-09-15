package currexx.algorithms.operators

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import currexx.algorithms.operators.species.{Distance, SpeciesOperators}
import kirill5k.common.cats.test.IOWordSpec

class ValidatorSpec extends IOWordSpec {

  /** A validator over a lookup table, plus the record of which individuals it actually scored. */
  private def validatorOver(scores: Map[String, Double], shortlistSize: Int = 3) =
    Ref.of[IO, List[String]](Nil).flatMap { scored =>
      Validator
        .shortlisted[IO, String](shortlistSize, ind => scored.update(_ :+ ind).as(Fitness(scores.getOrElse(ind, 0.0))))
        .map(_ -> scored)
    }

  private def trained(members: (String, Double)*): EvaluatedPopulation[String] =
    members.toVector.map { case (individual, fitness) => (individual, Fitness(fitness)) }

  private def speciesValidatorOver(scores: Map[String, Double], shortlistSize: Int = 4) =
    val familyDistance = new Distance[String] {
      override def between(a: String, b: String): Either[IllegalArgumentException, Double] = Right(if (a.head == b.head) 0.0 else 1.0)
    }
    for
      scored    <- Ref.of[IO, List[String]](Nil)
      species   <- SpeciesOperators.make[IO, String](familyDistance)
      validator <- Validator.speciesShortlisted[IO, String](
        shortlistSize,
        species,
        0.15,
        8,
        individual => scored.update(_ :+ individual).as(Fitness(scores.getOrElse(individual, 0.0)))
      )
    yield validator -> scored

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

  "Validator.speciesShortlisted" should {
    val population = trained("a" -> 10.0, "a2" -> 9.0, "a3" -> 8.0, "b" -> 2.0, "c" -> 1.0)

    "reserve final species representatives and spend the remaining slots on globally strong candidates" in
      speciesValidatorOver(Map.empty)
        .flatMap { case (validator, scored) =>
          validator.validate(population).flatMap(result => scored.get.map(result -> _))
        }
        .asserting { case (validated, scored) =>
          scored mustBe List("a", "a2", "b", "c")
          validated.map(_._1) mustBe Vector("a", "a2", "b", "c")
          validated.map(_._2) mustBe Vector(Fitness(10.0), Fitness(9.0), Fitness(2.0), Fitness(1.0))
        }

    "deduplicate before reserving and filling without treating zero distance as candidate equality" in
      speciesValidatorOver(Map.empty)
        .flatMap { case (validator, scored) =>
          validator.validate(population.head +: population).flatMap(_ => scored.get)
        }
        .asserting(_ mustBe List("a", "a2", "b", "c"))

    "sort current training scores before partitioning even when the supplied order is stale" in
      speciesValidatorOver(Map.empty)
        .flatMap { case (validator, _) => validator.validate(population.reverse) }
        .asserting(_.map(_._1) mustBe Vector("a", "a2", "b", "c"))

    "keep the hard limit when there are more species than available shortlist slots" in
      speciesValidatorOver(Map.empty, shortlistSize = 2)
        .flatMap { case (validator, scored) => validator.validate(population).flatMap(_ => scored.get) }
        .asserting(_ mustBe List("a", "b"))

    "apply the same validation gate and near-tie training preference as ordinary shortlisting" in
      speciesValidatorOver(Map("a" -> 0.50, "b" -> 0.51))
        .flatMap { case (validator, _) => validator.validate(population) }
        .asserting { validated =>
          validated.map(_._1) mustBe Vector("a", "b", "a2", "c")
          validated.map(_._3) mustBe Vector(Fitness(0.50), Fitness(0.51), Fitness(0.0), Fitness(0.0))
        }

    "validate each available candidate only once when the population is smaller than the limit" in
      speciesValidatorOver(Map.empty, shortlistSize = 25)
        .flatMap { case (validator, scored) => validator.validate(population).flatMap(_ => scored.get) }
        .asserting(_ mustBe List("a", "a2", "a3", "b", "c"))

    "avoid consulting held-out evidence for empty populations or zero-sized shortlists" in
      speciesValidatorOver(Map.empty, shortlistSize = 0)
        .flatMap { case (validator, scored) =>
          for
            emptyResult <- validator.validate(Vector.empty)
            zero        <- validator.validate(population)
            calls       <- scored.get
          yield (emptyResult, zero, calls)
        }
        .asserting { case (emptyResult, zero, calls) =>
          emptyResult mustBe Vector.empty
          zero mustBe Vector.empty
          calls mustBe empty
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
              .shortlisted[IO, String](3, ind => scored.update(_ :+ ind).as(Fitness(scores.getOrElse(ind, 0.0))), band)
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
