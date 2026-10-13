package currexx.backtest.optimizer

import cats.MonadThrow
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import cats.syntax.traverseFilter.*
import currexx.algorithms.{EvaluatedPopulation, Fitness}
import currexx.algorithms.operators.species.{Distance, Speciation}
import currexx.backtest.TestStrategy

/** Builds the bounded training-ranked shortlist before any validation evidence is read. */
final class FinalistAssembler[F[_]] private (
    space: OptimisationSearchSpace,
    baselines: FinalistAssembler.Baselines,
    rescore: TestStrategy => F[(TestStrategy, Fitness)],
    reserveRepresentatives: (EvaluatedPopulation[TestStrategy], Set[TestStrategy]) => F[Vector[TestStrategy]]
)(using F: MonadThrow[F]) {

  def shortlistSize: Int = baselines.shortlistSize

  def assemble(
      finalPopulation: EvaluatedPopulation[TestStrategy],
      archivedCandidates: EvaluatedPopulation[TestStrategy]
  ): F[EvaluatedPopulation[TestStrategy]] =
    for
      canonical <- canonicalise(finalPopulation ++ archivedCandidates)
      knownCandidates = canonical.map(_._1).toSet
      missing         = baselines.candidates.filterNot(knownCandidates.contains)
      rescored <- missing.traverse(rescore).flatMap(canonicalise)
      combined = ranked(canonical ++ rescored).distinctBy(_._1)
      representatives <- reserveRepresentatives(combined, baselines.candidateSet)
      selected = (baselines.candidates ++ representatives ++ combined.map(_._1)).distinct.take(shortlistSize).toSet
    yield combined.filter(member => selected.contains(member._1))

  // Incoming populations and rescoring are external boundaries; restore fixed inputs before comparing their identities.
  private def canonicalise(population: EvaluatedPopulation[TestStrategy]): F[EvaluatedPopulation[TestStrategy]] =
    F.fromEither(population.map { case (candidate, fitness) => space.canonicalise(candidate).map(_ -> fitness) }.sequence)

  private def ranked(population: EvaluatedPopulation[TestStrategy]): EvaluatedPopulation[TestStrategy] =
    population.sortBy { case (candidate, fitness) =>
      (-fitness.value, baselines.priorities.getOrElse(candidate, Int.MaxValue), candidate.toString)
    }
}

object FinalistAssembler {

  /** Canonical target and effective seeds with their already-validated shortlist budget. */
  final class Baselines private[FinalistAssembler] (val candidates: Vector[TestStrategy], val shortlistSize: Int) {
    private[FinalistAssembler] val priorities: Map[TestStrategy, Int] = candidates.zipWithIndex.toMap
    private[FinalistAssembler] val candidateSet: Set[TestStrategy]    = candidates.toSet
  }

  /** Seed identities follow the same fixed-input restoration and compatibility rules as initialisation. */
  def protectedBaselines(
      space: OptimisationSearchSpace,
      extraSeeds: List[TestStrategy],
      shortlistSize: Int
  ): Either[Throwable, Baselines] =
    for
      target <- space.canonicalise(space.template)
      seeds  <- space.resolveSeeds(extraSeeds)
      baselines = (target +: seeds.flatMap(_.effective).toVector).distinct
      _ <- validateBudget(baselines, shortlistSize)
    yield new Baselines(baselines, shortlistSize)

  def ga[F[_]: MonadThrow](
      space: OptimisationSearchSpace,
      baselines: Baselines,
      rescore: TestStrategy => F[(TestStrategy, Fitness)]
  ): FinalistAssembler[F] =
    new FinalistAssembler(space, baselines, rescore, (_, _) => MonadThrow[F].pure(Vector.empty))

  def scga[F[_]: MonadThrow](
      space: OptimisationSearchSpace,
      baselines: Baselines,
      rescore: TestStrategy => F[(TestStrategy, Fitness)],
      speciation: Speciation[F, TestStrategy],
      distance: Distance[TestStrategy],
      radius: Double,
      maxSpecies: Int
  ): FinalistAssembler[F] =
    new FinalistAssembler(
      space,
      baselines,
      rescore,
      (population, protectedCandidates) =>
        for
          groups    <- speciation.partition(population, radius, maxSpecies).flatMap(MonadThrow[F].fromEither)
          uncovered <- MonadThrow[F].fromEither {
            groups.species.filterA { group =>
              // At the species cap, distant members are assigned to the nearest group without satisfying its radius.
              group.members
                .filter(member => protectedCandidates.contains(member._1))
                .existsM { case (candidate, _) =>
                  distance.between(candidate, group.representative._1).map(_ <= radius)
                }
                .map(!_)
            }
          }
        yield uncovered
          .sortBy(group => (-group.representative._2.value, group.representative._1.toString))
          .map(_.representative._1)
    )

  private def validateBudget(baselines: Vector[TestStrategy], shortlistSize: Int): Either[IllegalArgumentException, Unit] =
    if (shortlistSize <= 0) Left(new IllegalArgumentException("Finalist shortlist size must be positive"))
    else if (baselines.size > shortlistSize)
      Left(
        new IllegalArgumentException(
          s"Finalist shortlist size $shortlistSize cannot hold ${baselines.size} distinct baselines; increase shortlistSize or remove extra seeds"
        )
      )
    else Right(())
}
