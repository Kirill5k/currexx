package currexx.backtest.optimizer

import cats.{FlatMap, Parallel}
import cats.effect.Async
import cats.syntax.all.*
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.Tracker
import currexx.algorithms.*
import currexx.backtest.{OptimisationRound, OrderStats, StrategyCatalogue}
import currexx.backtest.optimizer.reporting.{OptimisationReportBuilder, ReportingTracker, RunDiagnostics}
import currexx.domain.signal.Indicator

import scala.util.Random

trait OptimisationAlgorithm[F[_], A <: Alg, P <: Parameters[A], T]:
  def optimise(target: T, params: P)(using rand: Random): F[ValidatedPopulation[T]]

/** A configured indicator search that appends diagnostics after selection, retaining explicit replay access for callers. */
trait IndicatorOptimisation[F[_]]:
  def optimise(using Random): F[OptimisationResult]
  def searchSpace: IndicatorSearchSpace
  def tracker: Tracker[F, Indicator]
  def validate(indicator: Indicator): F[List[OrderStats]]

object OptimisationAlgorithm:
  def indicator[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      evaluatorPoolSize: Int,
      catalogue: List[StrategyCatalogue.Entry] = StrategyCatalogue.entries
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      _ <- Async[F].raiseWhen(round.corpus.validationFold.isEmpty)(
        new IllegalArgumentException("Strategy upgrade decisions require a nonempty selection period")
      )
      space     <- Async[F].fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
      baselines <- Async[F].fromEither(
        FinalistAssembler.protectedBaselines(space, round.extraSeeds.map(_.indicator), round.shortlistSize)
      )
      diagnostics <- RunDiagnostics.make[F]
      markdown    <- Tracker.markdown[F, Indicator](
        algorithmName = round.parameters.name,
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = baselines.shortlistSize
      )
      logging <- Tracker.logging[F, Indicator](
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = baselines.shortlistSize
      )
      progressTracker = new ReportingTracker(
        Tracker.composite(markdown, logging),
        diagnostics,
        round.corpus.foldCount,
        searchObjective = round.searchObjective
      )
      algorithm <- round.parameters match
        case params: Parameters.GA   => ga[F](round, params, space, baselines, progressTracker, evaluatorPoolSize, diagnostics, catalogue)
        case params: Parameters.SCGA => scga[F](round, params, space, baselines, progressTracker, evaluatorPoolSize, diagnostics, catalogue)
    yield algorithm

  private def ga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.GA,
      space: IndicatorSearchSpace,
      baselines: FinalistAssembler.Baselines,
      progressTracker: ReportingTracker[F],
      evaluatorPoolSize: Int,
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      search    <- IndicatorSearchOperators.make[F](space, round.extraSeeds.map(_.indicator))
      selector  <- Selector.tournament[F, Indicator]
      elitism   <- Elitism.simple[F, Indicator]
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space),
        diagnostics = Some(diagnostics),
        searchObjective = round.searchObjective,
        upgradePolicy = round.upgradePolicy,
        baseIndicator = Some(round.strategy.indicator)
      )
      assembler = FinalistAssembler.ga(
        space,
        baselines,
        objective.evaluator.evaluateIndividual(_, EvaluationPhase.Rescore)
      )
      buildAlgorithm = (evaluator: Evaluator[F, Indicator], validator: Validator[F, Indicator]) =>
        ga[F, Indicator](
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

  private def scga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.SCGA,
      space: IndicatorSearchSpace,
      baselines: FinalistAssembler.Baselines,
      progressTracker: ReportingTracker[F],
      evaluatorPoolSize: Int,
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      search <- IndicatorSearchOperators.make[F](space, round.extraSeeds.map(_.indicator))
      distance = IndicatorDistance.make(space)
      species   <- SpeciesOperators.make[F, Indicator](distance)
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space),
        diagnostics = Some(diagnostics),
        searchObjective = round.searchObjective,
        upgradePolicy = round.upgradePolicy,
        baseIndicator = Some(round.strategy.indicator)
      )
      assembler = FinalistAssembler.scga(
        space,
        baselines,
        objective.evaluator.evaluateIndividual(_, EvaluationPhase.Rescore),
        species.speciation,
        distance,
        params.speciesRadius,
        params.effectiveMaxSpecies
      )
      buildAlgorithm = (evaluator: Evaluator[F, Indicator], validator: Validator[F, Indicator]) =>
        scga[F, Indicator](
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
      validator: Validator[F, Indicator]
  ): Validator[F, Indicator] = new Validator[F, Indicator]:
    override def validate(population: EvaluatedPopulation[Indicator]): F[ValidatedPopulation[Indicator]] =
      archive.candidates
        .flatMap(assembler.assemble(population, _))
        .flatMap(validator.validate)

  private def prepared[F[_]: Async, A <: Alg, P <: Parameters[A]](
      round: OptimisationRound,
      params: P,
      space: IndicatorSearchSpace,
      objective: IndicatorObjective.Operators[F],
      assembler: FinalistAssembler[F],
      buildAlgorithm: (Evaluator[F, Indicator], Validator[F, Indicator]) => OptimisationAlgorithm[F, A, P, Indicator],
      progressTracker: ReportingTracker[F],
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  ): IndicatorOptimisation[F] = new IndicatorOptimisation[F]:
    override def optimise(using Random): F[OptimisationResult] =
      for
        started   <- Async[F].monotonic
        archive   <- SearchArchive.make[F](assembler.shortlistSize)
        validator <- Validator.shortlisted[F, Indicator](assembler.shortlistSize, objective.validationObjective)
        algorithm = buildAlgorithm(objective.evaluator.observingSearch(archive.record), assemblingValidator(archive, assembler, validator))
        finalists <- algorithm.optimise(round.strategy.indicator, params)
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
    override val searchSpace: IndicatorSearchSpace                   = space
    override val tracker: Tracker[F, Indicator]                      = progressTracker
    override def validate(indicator: Indicator): F[List[OrderStats]] = objective.validate(indicator)

  def scga[F[_]: Async, T](
      initialiser: Initialiser[F, T],
      crossover: Crossover[F, T],
      mutator: Mutator[F, T],
      evaluator: Evaluator[F, T],
      validator: Validator[F, T],
      species: SpeciesOperators[F, T],
      progressTracker: Tracker[F, T]
  ): OptimisationAlgorithm[F, Alg.SCGA, Parameters.SCGA, T] = new OptimisationAlgorithm[F, Alg.SCGA, Parameters.SCGA, T]:
    override def optimise(target: T, params: Parameters.SCGA)(using rand: Random): F[ValidatedPopulation[T]] =
      Algorithm.SCGA
        .optimise[T](target, params)
        .foldMap(Op.scgaInterpreter(initialiser, crossover, mutator, evaluator, validator, species, progressTracker))

  def ga[F[_]: Async, T](
      initialiser: Initialiser[F, T],
      crossover: Crossover[F, T],
      mutator: Mutator[F, T],
      evaluator: Evaluator[F, T],
      validator: Validator[F, T],
      selector: Selector[F, T],
      elitism: Elitism[F, T],
      progressTracker: Tracker[F, T]
  ): OptimisationAlgorithm[F, Alg.GA, Parameters.GA, T] = new OptimisationAlgorithm[F, Alg.GA, Parameters.GA, T]:
    override def optimise(target: T, params: Parameters.GA)(using rand: Random): F[ValidatedPopulation[T]] =
      Algorithm.GA
        .optimise[T](target, params)
        .foldMap(Op.ioInterpreter[F, T](initialiser, crossover, mutator, evaluator, validator, selector, elitism, progressTracker))
