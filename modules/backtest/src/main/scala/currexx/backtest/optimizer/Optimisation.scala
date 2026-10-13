package currexx.backtest.optimizer

import cats.{FlatMap, Parallel}
import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.algorithms.*
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.Tracker
import currexx.backtest.{OrderStats, StrategyCatalogue, TestStrategy}
import currexx.backtest.optimizer.reporting.{CandidateDisplay, OptimisationReportBuilder, ReportingTracker, RunDiagnostics}

import scala.util.Random

/** A configured search that appends diagnostics after selection, retaining explicit replay access for callers. */
trait Optimisation[F[_]]:
  def optimise(using Random): F[OptimisationResult]
  def searchSpace: OptimisationSearchSpace
  def tracker: Tracker[F, TestStrategy]
  def validate(candidate: TestStrategy): F[List[OrderStats]]

object Optimisation {
  def make[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      evaluatorPoolSize: Int,
      catalogue: List[StrategyCatalogue.Entry] = StrategyCatalogue.entries
  )(using Random): F[Optimisation[F]] =
    for
      _ <- Async[F].raiseWhen(round.corpus.validationFold.isEmpty)(
        new IllegalArgumentException("Strategy upgrade decisions require a nonempty selection period")
      )
      space     <- Async[F].fromEither(OptimisationSearchSpace.forRound(round))
      baselines <- Async[F].fromEither(
        FinalistAssembler.protectedBaselines(space, round.extraSeeds.map(_.strategy), round.shortlistSize)
      )
      diagnostics <- RunDiagnostics.make[F]
      search      <- OptimisationSearchOperators.make[F](space, round.extraSeeds.map(_.strategy))
      objective   <- OptimisationObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space),
        diagnostics = Some(diagnostics),
        searchObjective = round.searchObjective,
        upgradePolicy = round.upgradePolicy
      )
      renderCandidate = CandidateDisplay.forTarget(round.target)
      markdown <- Tracker.markdown[F, TestStrategy](
        algorithmName = round.parameters.name,
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = baselines.shortlistSize,
        renderIndividual = renderCandidate
      )
      logging <- Tracker.logging[F, TestStrategy](
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = baselines.shortlistSize,
        renderIndividual = renderCandidate
      )
      progressTracker = new ReportingTracker(
        Tracker.composite(markdown, logging),
        diagnostics,
        round.corpus.foldCount,
        searchObjective = round.searchObjective,
        renderCandidate = renderCandidate
      )
      algorithm <- round.parameters match
        case params: Parameters.GA   => ga[F](round, params, space, baselines, search, objective, progressTracker, diagnostics, catalogue)
        case params: Parameters.SCGA => scga[F](round, params, space, baselines, search, objective, progressTracker, diagnostics, catalogue)
    yield algorithm

  private def ga[F[_]: Async](
      round: OptimisationRound,
      params: Parameters.GA,
      space: OptimisationSearchSpace,
      baselines: FinalistAssembler.Baselines,
      search: OptimisationSearchOperators[F],
      objective: OptimisationObjective.Operators[F],
      progressTracker: ReportingTracker[F],
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  ): F[Optimisation[F]] =
    for
      selector <- Selector.tournament[F, TestStrategy]
      elitism  <- Elitism.simple[F, TestStrategy]
      assembler = FinalistAssembler.ga(
        space,
        baselines,
        objective.evaluator.evaluateIndividual(_, EvaluationPhase.Rescore)
      )
      buildAlgorithm = (evaluator: Evaluator[F, TestStrategy], validator: Validator[F, TestStrategy]) =>
        OptimisationAlgorithm.ga[F, TestStrategy](
          search.initialiser,
          search.crossover,
          search.mutator,
          evaluator,
          validator,
          selector,
          elitism,
          progressTracker
        )
    yield prepared(round, params, space, objective, assembler, buildAlgorithm, progressTracker, diagnostics, catalogue)

  private def scga[F[_]: Async](
      round: OptimisationRound,
      params: Parameters.SCGA,
      space: OptimisationSearchSpace,
      baselines: FinalistAssembler.Baselines,
      search: OptimisationSearchOperators[F],
      objective: OptimisationObjective.Operators[F],
      progressTracker: ReportingTracker[F],
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  ): F[Optimisation[F]] =
    for
      species <- SpeciesOperators.make[F, TestStrategy](search.distance)
      assembler = FinalistAssembler.scga(
        space,
        baselines,
        objective.evaluator.evaluateIndividual(_, EvaluationPhase.Rescore),
        species.speciation,
        search.distance,
        params.speciesRadius,
        params.effectiveMaxSpecies
      )
      buildAlgorithm = (evaluator: Evaluator[F, TestStrategy], validator: Validator[F, TestStrategy]) =>
        OptimisationAlgorithm.scga[F, TestStrategy](
          search.initialiser,
          search.crossover,
          search.mutator,
          evaluator,
          validator,
          species,
          progressTracker
        )
    yield prepared(round, params, space, objective, assembler, buildAlgorithm, progressTracker, diagnostics, catalogue)

  private[optimizer] def assemblingValidator[F[_]: FlatMap](
      archive: SearchArchive[F],
      assembler: FinalistAssembler[F],
      validator: Validator[F, TestStrategy]
  ): Validator[F, TestStrategy] = new Validator[F, TestStrategy]:
    override def validate(population: EvaluatedPopulation[TestStrategy]): F[ValidatedPopulation[TestStrategy]] =
      archive.candidates
        .flatMap(assembler.assemble(population, _))
        .flatMap(validator.validate)

  private def prepared[F[_]: Async, A <: Alg, P <: Parameters[A]](
      round: OptimisationRound,
      params: P,
      space: OptimisationSearchSpace,
      objective: OptimisationObjective.Operators[F],
      assembler: FinalistAssembler[F],
      buildAlgorithm: (Evaluator[F, TestStrategy], Validator[F, TestStrategy]) => OptimisationAlgorithm[F, A, P, TestStrategy],
      progressTracker: ReportingTracker[F],
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  ): Optimisation[F] = new Optimisation[F]:
    override def optimise(using Random): F[OptimisationResult] =
      for
        started   <- Async[F].monotonic
        archive   <- SearchArchive.make[F](assembler.shortlistSize)
        validator <- Validator.shortlisted[F, TestStrategy](assembler.shortlistSize, objective.validationObjective)
        algorithm = buildAlgorithm(objective.evaluator.observingSearch(archive.record), assemblingValidator(archive, assembler, validator))
        finalists <- algorithm.optimise(round.strategy, params)
        base      <- objective.selectionEvidence(space.template)
        evidence  <- finalists.toList.traverse(candidate => objective.selectionEvidence(candidate._1))
        decision  <- Async[F].fromEither(UpgradePolicy.decide(base, evidence, round.upgradePolicy))
        result = OptimisationResult(finalists, decision, base)
        finished <- Async[F].monotonic
        snapshot <- diagnostics.snapshot
        builder = new OptimisationReportBuilder(round, space, objective.inspect, diagnostics, catalogue)
        _ <- (progressTracker.displayCompletedRankings(finalists) >>
          builder.build(result, snapshot, finished - started).flatMap(progressTracker.displayReport))
          .handleErrorWith(error => progressTracker.displayReportFailure(round.name, error).attempt.void)
      yield result
    override val searchSpace: OptimisationSearchSpace                   = space
    override val tracker: Tracker[F, TestStrategy]                      = progressTracker
    override def validate(candidate: TestStrategy): F[List[OrderStats]] = objective.validate(candidate)
}
