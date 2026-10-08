package currexx.backtest.walkforward

import currexx.backtest.MarketDataProvider.DateRange

import java.time.YearMonth
import java.time.temporal.ChronoUnit

/** Calendar policy only. Neither market data nor optimisation results participate in scheduling. */
final case class WalkForwardPlan private (windows: List[WalkForwardWindow])

object WalkForwardPlan:
  val default: Either[IllegalArgumentException, WalkForwardPlan] = expanding(DateRange(YearMonth.of(2023, 7), YearMonth.of(2026, 7)))

  def expanding(
      history: DateRange,
      initialTrainingMonths: Int = 12,
      periodMonths: Int = 4
  ): Either[IllegalArgumentException, WalkForwardPlan] =
    if (!history.from.isBefore(history.until)) Left(new IllegalArgumentException("History must be a nonempty calendar range"))
    else if (periodMonths <= 0 || initialTrainingMonths <= 0 || initialTrainingMonths % periodMonths != 0)
      Left(new IllegalArgumentException("Initial training must contain a positive whole number of training folds"))
    else
      val months  = ChronoUnit.MONTHS.between(history.from, history.until)
      val count   = ((months - initialTrainingMonths - periodMonths) / periodMonths).max(0).toInt
      val windows = (0 until count).toList.map { index =>
        val selectionFrom = history.from.plusMonths(initialTrainingMonths.toLong + index.toLong * periodMonths)
        val testFrom      = selectionFrom.plusMonths(periodMonths)
        val folds         = Iterator
          .iterate(history.from)(_.plusMonths(periodMonths))
          .takeWhile(_.isBefore(selectionFrom))
          .map { from =>
            DateRange(from, from.plusMonths(periodMonths))
          }
          .toList
        WalkForwardWindow(index + 1, folds, DateRange(selectionFrom, testFrom), DateRange(testFrom, testFrom.plusMonths(periodMonths)))
      }
      validate(windows)

  def validate(windows: List[WalkForwardWindow]): Either[IllegalArgumentException, WalkForwardPlan] =
    def nonempty(range: DateRange): Boolean         = range.from.isBefore(range.until)
    def ordered(window: WalkForwardWindow): Boolean =
      window.trainingFolds.nonEmpty &&
        (window.trainingFolds ::: List(window.selection, window.test)).forall(nonempty) &&
        (window.trainingFolds ::: List(window.selection, window.test)).sliding(2).forall {
          case List(previous, next) => previous.until == next.from
          case _                    => true
        }
    def chronological = windows.sliding(2).forall {
      case List(previous, next) =>
        !previous.test.until.isAfter(next.test.from) && previous.training.from == next.training.from &&
        previous.training.until.isBefore(next.training.until)
      case _ => true
    }
    if (windows.isEmpty) Left(new IllegalArgumentException("History does not contain a complete training, selection and test window"))
    else if (!windows.forall(ordered))
      Left(new IllegalArgumentException("Each window needs contiguous, nonempty training, selection and test ranges"))
    else if (!chronological || windows.map(_.index) != (1 to windows.size).toList)
      Left(new IllegalArgumentException("Windows must expand training in chronological order with nonoverlapping tests"))
    else Right(WalkForwardPlan(windows))
