package currexx.backtest.walkforward

import cats.effect.Async
import cats.syntax.applicativeError.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.monadError.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider.{Corpus, Dataset, DateRange}

/** Application-owned persistence callbacks. Successful selection persistence is required before test evaluation. */
final case class WalkForwardEvents[F[_]](
    frozen: (WalkForwardWindow, Long, FrozenSelection) => F[Unit],
    completed: WindowResult => F[Unit],
    failed: (WalkForwardWindow, Throwable) => F[Unit],
    prepared: List[WindowReadiness] => F[Unit]
)

/** Runs one independent search and paired forward test at a time; test outcomes never enter subsequent search requests. */
final class WalkForwardRunner[F[_]](
    search: WindowSearch[F],
    forward: ForwardEvaluator[F],
    events: WalkForwardEvents[F]
)(using
    F: Async[F]
) {

  def run(experiment: WalkForwardExperiment): F[List[WindowResult]] = F.defer {
    preflight(experiment) >> experiment.plan.windows.traverse(runWindow(experiment, _))
  }

  private def preflight(experiment: WalkForwardExperiment): F[Unit] =
    val first = experiment.plan.windows.head
    F.defer {
      val history = experiment.history
      F.raiseUnless(
        history.nonEmpty && history.forall(_.range.isEmpty) &&
          history.map(dataset => (dataset.currencyPair, dataset.interval)).distinct.size == history.size
      )(new IllegalArgumentException("History must contain distinct, unranged pair/interval series"))
    }.flatMap(_ => WalkForwardPreflight.inspect[F](experiment.history, experiment.plan))
      .adaptError {
        case error: WalkForwardPreflight.Failure => error
        case error => new IllegalStateException(s"Window ${first.index} (history preflight) failed: ${error.getMessage}", error)
      }
      .flatMap(ready => stage(first, "persist preflight")(events.prepared(ready)))
      .handleErrorWith { error =>
        val window = error match
          case failure: WalkForwardPreflight.Failure => failure.window
          case _                                     => first
        reportFailure(window, error)
      }

  private def stage[A](window: WalkForwardWindow, name: String)(effect: F[A]): F[A] =
    effect.adaptError { case error => new IllegalStateException(s"Window ${window.index} ($name) failed: ${error.getMessage}", error) }

  private def runWindow(experiment: WalkForwardExperiment, window: WalkForwardWindow): F[WindowResult] =
    def datasets(range: DateRange): List[Dataset] = experiment.history.map(_.copy(range = Some(range)))

    val corpus = Corpus(window.trainingFolds.map(datasets), datasets(window.selection))
    val round  = experiment.round.copy(name = s"${experiment.id}-window-${window.index}", corpus = corpus)
    val seed   = window.seed(experiment.masterSeed)
    val run    = for
      optimisation <- stage(window, "search and selection")(search.search(round, seed))
      decision = FrozenSelection.fromResult(experiment.round.strategy, optimisation)
      _          <- stage(window, "persist selection")(events.frozen(window, seed, decision))
      evaluation <- stage(window, "forward evaluation")(
        forward.evaluate(decision.strategy, experiment.round.strategy, datasets(window.test))
      )
      result = WindowResult(window, seed, decision, evaluation)
      _ <- stage(window, "persist result")(events.completed(result))
    yield result
    run.handleErrorWith(reportFailure(window, _))

  private def reportFailure[A](window: WalkForwardWindow, error: Throwable): F[A] =
    events.failed(window, error).attempt.flatMap {
      case Left(reportError) => F.delay(error.addSuppressed(reportError)) >> F.raiseError(error)
      case Right(_)          => F.raiseError(error)
    }
}
