package currexx.backtest.optimizer

import cats.Parallel
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
  def optimise(using Random): F[ValidatedPopulation[Indicator]]
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
      diagnostics <- RunDiagnostics.make[F]
      markdown    <- Tracker.markdown[F, Indicator](
        algorithmName = round.parameters.name,
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = round.shortlistSize
      )
      logging <- Tracker.logging[F, Indicator](
        label = round.name,
        logInterval = 10,
        showTopMember = true,
        showTopN = 3,
        showStats = false,
        finalTopN = round.shortlistSize
      )
      progressTracker = new ReportingTracker(Tracker.composite(markdown, logging), diagnostics, round.corpus.foldCount)
      algorithm <- round.parameters match
        case params: Parameters.GA   => ga[F](round, params, progressTracker, evaluatorPoolSize, diagnostics, catalogue)
        case params: Parameters.SCGA => scga[F](round, params, progressTracker, evaluatorPoolSize, diagnostics, catalogue)
    yield algorithm

  private def ga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.GA,
      progressTracker: ReportingTracker[F],
      evaluatorPoolSize: Int,
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      space     <- Async[F].fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
      search    <- IndicatorSearchOperators.make[F](space, round.extraSeeds.map(_.indicator))
      selector  <- Selector.tournament[F, Indicator]
      elitism   <- Elitism.simple[F, Indicator]
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space),
        diagnostics = Some(diagnostics)
      )
      validator <- Validator.shortlisted[F, Indicator](round.shortlistSize, objective.validationObjective)
      algorithm = ga[F, Indicator](
        search.initialiser,
        search.crossover,
        search.mutator,
        objective.evaluator,
        canonicalValidator(space, validator),
        selector,
        elitism,
        progressTracker
      )
    yield prepared(round, params, space, objective, algorithm, progressTracker, diagnostics, catalogue)

  private def scga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.SCGA,
      progressTracker: ReportingTracker[F],
      evaluatorPoolSize: Int,
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      space     <- Async[F].fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
      search    <- IndicatorSearchOperators.make[F](space, round.extraSeeds.map(_.indicator))
      species   <- SpeciesOperators.make[F, Indicator](IndicatorDistance.make(space))
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space),
        diagnostics = Some(diagnostics)
      )
      validator <- Validator.speciesShortlisted[F, Indicator](
        round.shortlistSize,
        species,
        params.speciesRadius,
        params.effectiveMaxSpecies,
        objective.validationObjective
      )
      algorithm = scga[F, Indicator](
        search.initialiser,
        search.crossover,
        search.mutator,
        objective.evaluator,
        canonicalValidator(space, validator),
        species,
        progressTracker
      )
    yield prepared(round, params, space, objective, algorithm, progressTracker, diagnostics, catalogue)

  private[optimizer] def canonicalValidator[F[_]: Async](
      space: IndicatorSearchSpace,
      validator: Validator[F, Indicator]
  ): Validator[F, Indicator] = new Validator[F, Indicator]:
    override def validate(population: EvaluatedPopulation[Indicator]): F[ValidatedPopulation[Indicator]] =
      // Canonicalise before deduplication and truncation, so fixed-only differences cannot consume shortlist slots.
      Async[F]
        .fromEither(population.map { case (ind, fitness) => space.canonicalise(ind).map(_ -> fitness) }.sequence)
        .flatMap(validator.validate)

  private def prepared[F[_]: Async, A <: Alg, P <: Parameters[A]](
      round: OptimisationRound,
      params: P,
      space: IndicatorSearchSpace,
      objective: IndicatorObjective.Operators[F],
      algorithm: OptimisationAlgorithm[F, A, P, Indicator],
      progressTracker: ReportingTracker[F],
      diagnostics: RunDiagnostics[F],
      catalogue: List[StrategyCatalogue.Entry]
  ): IndicatorOptimisation[F] = new IndicatorOptimisation[F]:
    override def optimise(using Random): F[ValidatedPopulation[Indicator]] =
      for
        started   <- Async[F].monotonic
        finalists <- algorithm.optimise(round.strategy.indicator, params)
        finished  <- Async[F].monotonic
        snapshot  <- diagnostics.snapshot
        builder = new OptimisationReportBuilder(round, space, objective.inspect, diagnostics, catalogue)
        _ <- builder
          .build(finalists, snapshot, finished - started)
          .flatMap(progressTracker.displayReport)
          .handleErrorWith(error => progressTracker.displayReportFailure(round.name, error).attempt.void)
      yield finalists
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
