package currexx.backtest

import currexx.backtest.syntax.*

import java.time.{Instant, YearMonth, ZoneOffset}

private[backtest] object EquityCurve {

  /** Values are chronological, with one account value per timestamp. The starting balance is the first high-water mark. */
  def fromValues(values: List[(Instant, BigDecimal)], initialBalance: BigDecimal): List[EquityPoint] =
    values
      .foldLeft((initialBalance, List.empty[EquityPoint])) { case ((peak, points), (time, equity)) =>
        val nextPeak = peak.max(equity)
        val drawdown = nextPeak - equity
        val percent  = if (nextPeak == 0) BigDecimal(0) else (drawdown / nextPeak * 100).roundTo(8)
        (nextPeak, EquityPoint(time, equity, drawdown, percent) :: points)
      }
      ._2
      .reverse

  /** Sum changes at the same instant before measuring portfolio drawdown. Between observations a member retains its last equity; before its
    * first observation it contributes its initial balance, and after its last it retains its terminal balance.
    */
  def combine(stats: List[OrderStats], initialBalance: BigDecimal): List[EquityPoint] = {
    val changes = stats
      .flatMap { member =>
        member.equityCurve
          .foldLeft((member.initialBalance, List.empty[(Instant, BigDecimal)])) { case ((previous, acc), point) =>
            (point.equity, (point.time -> (point.equity - previous)) :: acc)
          }
          ._2
      }
      .groupMapReduce(_._1)(_._2)(_ + _)
      .toList
      .sortBy(_._1)
    val values = changes
      .foldLeft((initialBalance, List.empty[(Instant, BigDecimal)])) { case ((equity, acc), (time, change)) =>
        val next = equity + change
        (next, (time -> next) :: acc)
      }
      ._2
      .reverse
    fromValues(values, initialBalance)
  }

  /** Calendar profit is the change between successive month-end equity values, including open P&L. Months with no observations carry the
    * previous equity forward. A partial first or last month uses the supplied data window without extrapolating returns.
    */
  def monthlyProfits(
      curve: List[EquityPoint],
      initialBalance: BigDecimal,
      window: Option[DataWindow]
  ): Map[String, BigDecimal] = {
    val bounds = curve.headOption.map(_.time).toList ::: curve.lastOption.map(_.time).toList :::
      window.toList.flatMap(w => List(w.from, w.to))
    bounds match {
      case Nil   => Map.empty
      case times =>
        val first         = YearMonth.from(times.min.atZone(ZoneOffset.UTC))
        val last          = YearMonth.from(times.max.atZone(ZoneOffset.UTC))
        val closingEquity = curve.foldLeft(Map.empty[YearMonth, BigDecimal]) { (months, point) =>
          months.updated(YearMonth.from(point.time.atZone(ZoneOffset.UTC)), point.equity)
        }
        Iterator
          .iterate(first)(_.plusMonths(1))
          .takeWhile(!_.isAfter(last))
          .foldLeft(
            (initialBalance, Map.empty[String, BigDecimal])
          ) { case ((previous, profits), month) =>
            val equity = closingEquity.getOrElse(month, previous)
            (equity, profits.updated(month.toString, equity - previous))
          }
          ._2
    }
  }
}
