package currexx.backtest.walkforward

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.traverse.*
import currexx.backtest.MarketDataProvider
import currexx.backtest.MarketDataProvider.{Dataset, DateRange, HistoryCoverage}

final case class PeriodReadiness(
    stage: String,
    range: DateRange,
    currencyPair: String,
    priceBars: Long,
    priorBars: Long,
    warmupBarsLost: Int
):
  // Every period also consumes one complete window to prime a fresh simulator.
  def executableBars: Long = priceBars - warmupBarsLost - 1

final case class WindowReadiness(window: WalkForwardWindow, periods: List[PeriodReadiness])

/** Check the entire schedule before spending search budget. Only availability metadata leaves the history scan. */
object WalkForwardPreflight:
  final class Failure(val window: WalkForwardWindow, cause: IllegalArgumentException)
      extends IllegalArgumentException(s"Window ${window.index} (history preflight) failed: ${cause.getMessage}", cause)

  def inspect[F[_]: Async](history: List[Dataset], plan: WalkForwardPlan): F[List[WindowReadiness]] =
    history.traverse(MarketDataProvider.inspectHistory[F]).flatMap(coverage => Async[F].fromEither(validate(coverage, plan)))

  def validate(history: List[HistoryCoverage], plan: WalkForwardPlan): Either[Failure, List[WindowReadiness]] =
    plan.windows.traverse { window =>
      val ranges = window.trainingFolds.zipWithIndex.map { case (range, index) => s"training fold ${index + 1}" -> range } :::
        List("selection" -> window.selection, "forward test" -> window.test)
      ranges
        .traverse { case (stage, range) =>
          history.traverse { coverage =>
            val months    = Iterator.iterate(range.from)(_.plusMonths(1)).takeWhile(_.isBefore(range.until)).toList
            val missing   = months.filter(month => coverage.perMonth.getOrElse(month, 0L) == 0)
            val priorBars = coverage.countBefore(range.from)
            val priceBars = coverage.countIn(range)
            val lost      = math.min(priceBars, math.max(0L, MarketDataProvider.priceWindowSize - 1L - priorBars)).toInt
            val ready     = PeriodReadiness(stage, range, coverage.currencyPair.toString, priceBars, priorBars, lost)
            val context   = s"$stage ($range), ${coverage.currencyPair} ${coverage.interval}"
            if (missing.nonEmpty)
              Left(new Failure(window, new IllegalArgumentException(s"Missing requested months for $context: ${missing.mkString(", ")}")))
            else if (ready.executableBars < 1)
              Left(new Failure(window, new IllegalArgumentException(s"Insufficient bars for warm-up, priming and execution in $context")))
            else Right(ready)
          }
        }
        .map(periods => WindowReadiness(window, periods.flatten))
    }
