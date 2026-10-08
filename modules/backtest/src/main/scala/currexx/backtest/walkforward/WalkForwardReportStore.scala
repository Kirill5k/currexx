package currexx.backtest.walkforward

import cats.effect.{Async, Ref}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider
import currexx.backtest.walkforward.WalkForwardReportRenderer.Failure
import fs2.io.file.Path
import io.circe.Json

import java.nio.charset.StandardCharsets
import java.nio.file.{AtomicMoveNotSupportedException, Files, StandardCopyOption}

final case class WalkForwardReportStore[F[_]](
    directory: Path,
    events: WalkForwardEvents[F]
)

/** Owns durable records only. A callback completes only after its required files have been written. */
object WalkForwardReportStore:
  val outputDirectory: Path = Path("walk-forward-results")

  def make[F[_]](experiment: WalkForwardExperiment, root: Path = outputDirectory)(using F: Async[F]): F[WalkForwardReportStore[F]] =
    val directory = root / experiment.id

    def write(name: String, content: String): F[Unit] = F.blocking {
      val destination = directory.toNioPath.resolve(name)
      val temporary   = directory.toNioPath.resolve(s".$name.tmp")
      Files.writeString(temporary, content, StandardCharsets.UTF_8)
      try Files.move(temporary, destination, StandardCopyOption.ATOMIC_MOVE, StandardCopyOption.REPLACE_EXISTING)
      catch case _: AtomicMoveNotSupportedException => Files.move(temporary, destination, StandardCopyOption.REPLACE_EXISTING)
      ()
    }
    def writeJson(name: String, content: Json): F[Unit] = write(name, content.spaces2 + "\n")
    def writeProgress(results: List[WindowResult], failure: Option[Failure], readiness: List[WindowReadiness] = Nil): F[Unit] =
      val totals = WalkForwardSummary.from(results)
      writeJson("summary.json", WalkForwardReportRenderer.progress(experiment, totals, failure)) >>
        write("report.md", WalkForwardReportRenderer.markdown(experiment, results, totals, failure, readiness))

    for
      fingerprints <- experiment.history
        .flatMap(_.filePaths.toList)
        .distinct
        .traverse(file => MarketDataProvider.fingerprint[F](file).tupleLeft(file))
      _ <- F.blocking {
        Option(directory.toNioPath.getParent).foreach(Files.createDirectories(_))
        Files.createDirectory(directory.toNioPath)
      }
      _         <- writeJson("manifest.json", WalkForwardReportRenderer.manifest(experiment, fingerprints))
      _         <- writeProgress(Nil, None)
      results   <- Ref.of[F, List[WindowResult]](Nil)
      readiness <- Ref.of[F, List[WindowReadiness]](Nil)
    yield WalkForwardReportStore(
      directory,
      WalkForwardEvents(
        frozen = (window, seed, selection) =>
          writeJson(s"window-${window.index}-frozen.json", WalkForwardReportRenderer.frozen(window, seed, selection)),
        completed = result =>
          writeJson(s"window-${result.window.index}-result.json", WalkForwardReportRenderer.completed(result)) >>
            results.updateAndGet(_ :+ result).flatMap(completed => readiness.get.flatMap(writeProgress(completed, None, _))),
        failed = (window, error) =>
          val failure = Failure(window, rootCause(error).getClass.getName, Option(error.getMessage).getOrElse("No error message"))
          writeJson(s"window-${window.index}-failure.json", WalkForwardReportRenderer.failed(failure)) >>
            results.get.flatMap(completed => readiness.get.flatMap(writeProgress(completed, Some(failure), _)))
        ,
        prepared = prepared =>
          writeJson("preflight.json", WalkForwardReportRenderer.preflight(prepared)) >>
            readiness.set(prepared) >> results.get.flatMap(writeProgress(_, None, prepared))
      )
    )

  private def rootCause(error: Throwable): Throwable =
    @scala.annotation.tailrec
    def loop(current: Throwable, seen: Set[Throwable]): Throwable =
      Option(current.getCause).filterNot(seen.contains) match
        case Some(cause) => loop(cause, seen + cause)
        case None        => current
    loop(error, Set(error))
