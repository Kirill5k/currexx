package currexx.backtest.optimizer

import cats.Parallel
import cats.effect.Async
import cats.syntax.all.*
import currexx.algorithms.operators.*
import currexx.algorithms.operators.species.SpeciesOperators
import currexx.algorithms.progress.Tracker
import currexx.algorithms.*
import currexx.backtest.{OptimisationRound, OrderStats}
import currexx.domain.signal.Indicator

import scala.util.Random

trait OptimisationAlgorithm[F[_], A <: Alg, P <: Parameters[A], T]:
  def optimise(target: T, params: P)(using rand: Random): F[ValidatedPopulation[T]]

/** A configured indicator search, retaining the search space and validation backtest used by its reports. */
trait IndicatorOptimisation[F[_]]:
  def optimise(using Random): F[ValidatedPopulation[Indicator]]
  def searchSpace: IndicatorSearchSpace
  def tracker: Tracker[F, Indicator]
  def validate(indicator: Indicator): F[List[OrderStats]]

object OptimisationAlgorithm:
  def indicator[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      evaluatorPoolSize: Int
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      markdown <- Tracker.markdown[F, Indicator](
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
      progressTracker = Tracker.composite(markdown, logging)
      algorithm <- round.parameters match
        case params: Parameters.GA   => ga[F](round, params, progressTracker, evaluatorPoolSize)
        case params: Parameters.SCGA => scga[F](round, params, progressTracker, evaluatorPoolSize)
    yield algorithm

  private def ga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.GA,
      progressTracker: Tracker[F, Indicator],
      evaluatorPoolSize: Int
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      space     <- Async[F].fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
      search    <- IndicatorSearchOperators.make[F](space, round.extraSeeds)
      selector  <- Selector.tournament[F, Indicator]
      elitism   <- Elitism.simple[F, Indicator]
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space)
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
    yield prepared(round, params, space, objective, algorithm, progressTracker)

  private def scga[F[_]: {Async, Parallel}](
      round: OptimisationRound,
      params: Parameters.SCGA,
      progressTracker: Tracker[F, Indicator],
      evaluatorPoolSize: Int
  )(using Random): F[IndicatorOptimisation[F]] =
    for
      space     <- Async[F].fromEither(IndicatorSearchSpace.forStrategy(round.strategy, round.fixedIndicators))
      search    <- IndicatorSearchOperators.make[F](space, round.extraSeeds)
      species   <- SpeciesOperators.make[F, Indicator](IndicatorDistance.make(space))
      objective <- IndicatorObjective.make[F](
        corpus = round.corpus,
        strategy = round.strategy.rules,
        poolSize = evaluatorPoolSize,
        scoringFunction = round.scoringFunction,
        searchSpace = Some(space)
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
    yield prepared(round, params, space, objective, algorithm, progressTracker)

  private[optimizer] def canonicalValidator[F[_]: Async](
      space: IndicatorSearchSpace,
      validator: Validator[F, Indicator]
  ): Validator[F, Indicator] = new Validator[F, Indicator]:
    override def validate(population: EvaluatedPopulation[Indicator]): F[ValidatedPopulation[Indicator]] =
      // Canonicalise before deduplication and truncation, so fixed-only differences cannot consume shortlist slots.
      Async[F]
        .fromEither(population.map { case (ind, fitness) => space.canonicalise(ind).map(_ -> fitness) }.sequence)
        .flatMap(validator.validate)

  private def prepared[F[_], A <: Alg, P <: Parameters[A]](
      round: OptimisationRound,
      params: P,
      space: IndicatorSearchSpace,
      objective: IndicatorObjective.Operators[F],
      algorithm: OptimisationAlgorithm[F, A, P, Indicator],
      progressTracker: Tracker[F, Indicator]
  ): IndicatorOptimisation[F] = new IndicatorOptimisation[F]:
    override def optimise(using Random): F[ValidatedPopulation[Indicator]] =
      algorithm.optimise(round.strategy.indicator, params)
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
