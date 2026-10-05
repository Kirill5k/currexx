package currexx.backtest.optimizer

import cats.effect.{IO, Ref}
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import currexx.algorithms.operators.Validator
import currexx.algorithms.operators.species.{Distance, Speciation, SpeciesPopulation}
import currexx.backtest.TestStrategy
import currexx.core.trade.TradeStrategy
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation as VT}
import kirill5k.common.cats.test.IOWordSpec

class FinalistAssemblerSpec extends IOWordSpec {
  private def trend(period: Int): Indicator = Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(period))

  private def searchSpace(target: Indicator): IndicatorSearchSpace =
    IndicatorSearchSpace.forStrategy(TestStrategy(target, TradeStrategy(Nil, Nil))).fold(throw _, identity)

  private val target = trend(11)
  private val space  = searchSpace(target)

  private def trained(members: (Indicator, Double)*): EvaluatedPopulation[Indicator] =
    members.toVector.map { case (indicator, fitness) => indicator -> Fitness(fitness) }

  private val unexpectedRescore: Indicator => IO[(Indicator, Fitness)] =
    indicator => IO.raiseError(new IllegalStateException(s"Unexpected rescore: $indicator"))

  private def preparedBaselines(
      candidates: Vector[Indicator] = Vector(target),
      size: Int = 3,
      search: IndicatorSearchSpace = space
  ): FinalistAssembler.Baselines =
    FinalistAssembler.protectedBaselines(search, candidates.toList, size).fold(throw _, identity)

  private def assembler(baselines: Vector[Indicator] = Vector(target), size: Int = 3): FinalistAssembler[IO] =
    FinalistAssembler.ga(space, preparedBaselines(baselines, size), unexpectedRescore)

  private val familyDistance = new Distance[Indicator] {
    override def between(a: Indicator, b: Indicator): Either[IllegalArgumentException, Double] =
      (a, b) match
        case (Indicator.TrendChangeDetection(_, VT.SMA(ap)), Indicator.TrendChangeDetection(_, VT.SMA(bp))) =>
          Right(if (ap / 100 == bp / 100) 0.0 else 1.0)
        case _ => Left(new IllegalArgumentException("Expected SMA trend indicators"))
  }

  private def speciesAssembler(baselines: Vector[Indicator] = Vector(target), size: Int = 3): IO[FinalistAssembler[IO]] =
    Speciation.make[IO, Indicator](familyDistance).map { speciation =>
      FinalistAssembler.scga(space, preparedBaselines(baselines, size), unexpectedRescore, speciation, familyDistance, 0.0, 10)
    }

  "FinalistAssembler.protectedBaselines" should {
    "preserve target and configured seed order while excluding incompatible seeds and fixed-input aliases" in {
      val raw        = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
      val changedRaw = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(20))
      val base       = Indicator.compositeAnyOf(target, raw)
      val seed       = Indicator.compositeAnyOf(trend(30), raw)
      val other      = Indicator.compositeAnyOf(trend(40), raw)
      val seeds      = List(
        Indicator.compositeAnyOf(target, changedRaw),
        Indicator.compositeAllOf(target, raw),
        Indicator.compositeAnyOf(trend(30), changedRaw),
        seed,
        other,
        base
      )

      FinalistAssembler.protectedBaselines(searchSpace(base), seeds, 3).map(_.candidates) mustBe Right(Vector(base, seed, other))
    }

    "reject nonpositive budgets and budgets smaller than the distinct baselines" in {
      FinalistAssembler.protectedBaselines(space, Nil, 0).left.map(_.getMessage) mustBe
        Left("Finalist shortlist size must be positive")
      FinalistAssembler.protectedBaselines(space, List(trend(20), trend(30)), 2).left.map(_.getMessage) mustBe
        Left("Finalist shortlist size 2 cannot hold 3 distinct baselines; increase shortlistSize or remove extra seeds")
    }

    "retain the validated budget when baselines consume every place" in {
      val baselines = preparedBaselines(Vector(target, trend(20)), 2)
      baselines.candidates mustBe Vector(target, trend(20))
      baselines.shortlistSize mustBe 2
    }
  }

  "FinalistAssembler" should {
    "reserve low-training baselines inside the budget and fill from both the archive and final population" in {
      val seed = trend(20)
      assembler(Vector(target, seed), 4)
        .assemble(trained(trend(30) -> 8.0, target -> 0.2, trend(40) -> 7.0), trained(seed -> 0.1, trend(50) -> 9.0))
        .asserting(_ mustBe trained(trend(50) -> 9.0, trend(30) -> 8.0, target -> 0.2, seed -> 0.1))
    }

    "canonicalise and deduplicate fixed-input aliases before consuming slots or rescoring" in {
      val raw        = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(1))
      val changedRaw = Indicator.ValueTracking(ValueRole.Price, ValueSource.Close, VT.SMA(40))
      val base       = Indicator.compositeAnyOf(target, raw)
      val alias      = Indicator.compositeAnyOf(target, changedRaw)
      val discovered = Indicator.compositeAnyOf(trend(20), raw)
      val search     = searchSpace(base)
      FinalistAssembler
        .ga(search, preparedBaselines(Vector(alias, base), 3, search), unexpectedRescore)
        .assemble(trained(alias -> 1.0, discovered -> 2.0), trained(base -> 1.0, discovered -> 2.0))
        .asserting(_ mustBe trained(discovered -> 2.0, base -> 1.0))
    }

    "rescore only missing baselines once and reuse archived scores" in {
      val seed   = trend(20)
      val result = for
        rescored <- Ref.of[IO, Vector[Indicator]](Vector.empty)
        assembly = FinalistAssembler.ga[IO](
          space,
          preparedBaselines(Vector(target, seed)),
          indicator => rescored.update(_ :+ indicator).as(indicator -> Fitness(0.1))
        )
        assembled <- assembly.assemble(trained(trend(30) -> 3.0), trained(seed -> 2.0))
        calls     <- rescored.get
      yield assembled -> calls
      result.asserting { case (assembled, calls) =>
        assembled mustBe trained(trend(30) -> 3.0, seed -> 2.0, target -> 0.1)
        calls mustBe Vector(target)
      }
    }

    "allow baselines to consume the entire budget and return all available candidates in an undersized pool" in {
      val seed       = trend(20)
      val population = trained(trend(30) -> 3.0, target -> 0.2, seed -> 0.1)
      val result     = for
        full       <- assembler(Vector(target, seed), 2).assemble(population, Vector.empty)
        undersized <- assembler(Vector(target, seed), 25).assemble(population, Vector.empty)
      yield full -> undersized
      result.asserting { case (full, undersized) =>
        full mustBe trained(target -> 0.2, seed -> 0.1)
        undersized mustBe population
      }
    }

    "resolve fitness ties by target then configured seed order then canonical text independently of input order" in {
      val seeds       = Vector(trend(30), trend(20))
      val discoveries = Vector(trend(40), trend(50)).sortBy(_.toString)
      val population  = trained((discoveries.reverse ++ seeds.reverse :+ target).map(_ -> 1.0)*)
      val result      = for
        first  <- assembler(target +: seeds, 5).assemble(population, population.reverse)
        second <- assembler(target +: seeds, 5).assemble(population.reverse, population)
      yield first -> second
      result.asserting { case (first, second) =>
        first.map(_._1) mustBe Vector(target) ++ seeds ++ discoveries
        second mustBe first
      }
    }

    "propagate rescoring and candidate compatibility failures" in {
      val error        = new RuntimeException("Failed fold")
      val incompatible = Indicator.TrendChangeDetection(ValueSource.Close, VT.EMA(20))
      val result       = for
        failed <- FinalistAssembler
          .ga[IO](space, preparedBaselines(), _ => IO.raiseError(error))
          .assemble(Vector.empty, Vector.empty)
          .attempt
        invalid <- assembler().assemble(trained(incompatible -> 1.0), Vector.empty).attempt
      yield failed -> invalid
      result.asserting { case (failed, invalid) =>
        failed mustBe Left(error)
        invalid.left.map(_.getMessage) mustBe Left("Indicator does not match this round's search-space schema")
      }
    }

    "let a stronger validated incumbent defeat a discovery and validate each selected identity once" in {
      val discovered = trend(20)
      val result     = for
        scored    <- Ref.of[IO, Vector[Indicator]](Vector.empty)
        shortlist <- assembler(size = 2).assemble(trained(discovered -> 1.0, target -> 0.1), trained(discovered -> 1.0))
        validator <- Validator.shortlisted[IO, Indicator](2, ind => scored.update(_ :+ ind).as(Fitness(if (ind == target) 0.7 else 0.2)))
        validated <- validator.validate(shortlist)
        calls     <- scored.get
      yield validated -> calls
      result.asserting { case (validated, calls) =>
        validated.map(_._1) mustBe Vector(target, discovered)
        calls mustBe Vector(discovered, target)
      }
    }

    "preserve consensus tie-band, exact-tie and all-zero validation ordering" in {
      val first  = trend(20)
      val second = trend(30)
      val third  = trend(40)
      val result = for
        shortlist <- assembler(Vector(target, first), 4)
          .assemble(trained(third -> 1.0, second -> 1.0, first -> 1.0, target -> 1.0), Vector.empty)
        tied          <- Validator.shortlisted[IO, Indicator](4, _ => IO.pure(Fitness(0.5))).flatMap(_.validate(shortlist))
        zero          <- Validator.shortlisted[IO, Indicator](4, _ => IO.pure(Fitness(0.0))).flatMap(_.validate(shortlist))
        bandShortlist <- assembler(size = 3).assemble(trained(first -> 1.0, second -> 0.8, target -> 0.1), Vector.empty)
        band          <- Validator
          .shortlisted[IO, Indicator](3, ind => IO.pure(Fitness(if (ind == first) 0.50 else if (ind == target) 0.51 else 0.0)))
          .flatMap(_.validate(bandShortlist))
      yield (shortlist, tied, zero, band)
      result.asserting { case (shortlist, tied, zero, band) =>
        tied.map(_._1) mustBe shortlist.map(_._1)
        zero.map(_._1) mustBe shortlist.map(_._1)
        band.map(_._1) mustBe Vector(first, target, second)
      }
    }
  }

  "FinalistAssembler.scga" should {
    val population = trained(trend(12) -> 10.0, trend(13) -> 9.0, trend(110) -> 2.0, trend(210) -> 1.5, target -> 1.0)

    "reserve unrepresented species before globally stronger members of a baseline-covered species" in
      speciesAssembler()
        .flatMap(_.assemble(population, Vector.empty))
        .asserting(_ mustBe trained(trend(110) -> 2.0, trend(210) -> 1.5, target -> 1.0))

    "fill remaining capacity globally after baseline and species reservations" in
      speciesAssembler(size = 4)
        .flatMap(_.assemble(population, Vector.empty))
        .asserting(_ mustBe trained(trend(12) -> 10.0, trend(110) -> 2.0, trend(210) -> 1.5, target -> 1.0))

    "recognise seed-covered species and include species only present in the archive" in {
      val seed = trend(111)
      speciesAssembler(Vector(target, seed), 4)
        .flatMap(_.assemble(population.filterNot(_._1 == trend(210)) :+ (seed -> Fitness(0.5)), trained(trend(210) -> 1.5)))
        .asserting(_ mustBe trained(trend(12) -> 10.0, trend(210) -> 1.5, target -> 1.0, seed -> 0.5))
    }

    "reserve the strongest unrepresented species when there are fewer places than species" in
      speciesAssembler(size = 2)
        .flatMap(_.assemble(population.reverse, Vector.empty))
        .asserting(_ mustBe trained(trend(110) -> 2.0, target -> 1.0))

    "keep the strongest representative when a distant baseline is assigned to its species by the cap" in {
      val base       = trend(5)
      val strongest  = trend(20)
      val other      = trend(100)
      val search     = searchSpace(base)
      val distance   = IndicatorDistance.make(search)
      val candidates = trained(strongest -> 10.0, other -> 2.0, base -> 1.0)
      val baselines  = preparedBaselines(Vector(base), 2, search)

      distance.between(base, strongest).exists(_ > 0.15) mustBe true
      Speciation
        .make[IO, Indicator](distance)
        .flatMap { speciation =>
          FinalistAssembler
            .scga(search, baselines, unexpectedRescore, speciation, distance, 0.15, 2)
            .assemble(candidates, Vector.empty)
        }
        .asserting(_ mustBe trained(strongest -> 10.0, base -> 1.0))
    }

    "count a baseline exactly at the species radius as covering its representative" in {
      val base      = trend(5)
      val strongest = trend(20)
      val other     = trend(100)
      val search    = searchSpace(base)
      val distance  = IndicatorDistance.make(search)
      val radius    = distance.between(base, strongest).fold(throw _, identity)
      val baselines = preparedBaselines(Vector(base), 2, search)

      Speciation
        .make[IO, Indicator](distance)
        .flatMap { speciation =>
          FinalistAssembler
            .scga(search, baselines, unexpectedRescore, speciation, distance, radius, 2)
            .assemble(trained(strongest -> 10.0, other -> 2.0, base -> 1.0), Vector.empty)
        }
        .asserting(_ mustBe trained(other -> 2.0, base -> 1.0))
    }

    "propagate failures while checking whether a baseline covers a species" in {
      val error          = new IllegalArgumentException("Cannot compare baseline with representative")
      val failedDistance = new Distance[Indicator] {
        override def between(a: Indicator, b: Indicator): Either[IllegalArgumentException, Double] = Left(error)
      }

      Speciation
        .make[IO, Indicator](familyDistance)
        .flatMap { speciation =>
          FinalistAssembler
            .scga(space, preparedBaselines(), unexpectedRescore, speciation, failedDistance, 0.0, 10)
            .assemble(population, Vector.empty)
            .attempt
        }
        .asserting(_ mustBe Left(error))
    }

    "rank species reservations independently of the order returned by speciation" in
      Speciation
        .make[IO, Indicator](familyDistance)
        .flatMap { underlying =>
          val reversed = new Speciation[IO, Indicator] {
            override def partition(
                candidates: EvaluatedPopulation[Indicator],
                radius: Double,
                maxSpecies: Int
            ): IO[Either[IllegalArgumentException, SpeciesPopulation[Indicator]]] =
              underlying.partition(candidates, radius, maxSpecies).map(_.map(groups => groups.copy(species = groups.species.reverse)))
          }
          FinalistAssembler
            .scga(space, preparedBaselines(size = 2), unexpectedRescore, reversed, familyDistance, 0.0, 10)
            .assemble(population, Vector.empty)
        }
        .asserting(_ mustBe trained(trend(110) -> 2.0, target -> 1.0))

    "propagate invalid species settings without substituting a shortlist" in
      Speciation
        .make[IO, Indicator](familyDistance)
        .flatMap { speciation =>
          FinalistAssembler
            .scga(space, preparedBaselines(), unexpectedRescore, speciation, familyDistance, -1.0, 10)
            .assemble(population, Vector.empty)
            .attempt
        }
        .asserting(_.left.map(_.getMessage) mustBe Left("Species radius must be finite and nonnegative"))
  }
}
