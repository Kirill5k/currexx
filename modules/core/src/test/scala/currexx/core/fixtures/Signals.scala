package currexx.core.fixtures

import cats.data.NonEmptyList
import currexx.core.signal.Signal
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.signal.{Boundary, Condition, Direction, Indicator, ValueSource, ValueTransformation}
import currexx.domain.user.UserId
import kirill5k.common.syntax.time.*

import java.time.Instant

object Indicators {
  val trendChangeDetection = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.HMA(16))
  val linesCrossing        = Indicator.LinesCrossing(ValueSource.Close, ValueTransformation.EMA(22), ValueTransformation.EMA(14))
}

object Signals {
  val ts = Instant.now.truncatedToSeconds

  val trendDirectionChange = Condition.TrendDirectionChange(Direction.Downward, Direction.Upward, Some(1))
  val linesCrossing        = Condition.LinesCrossing(Direction.Upward)

  def make(
      uid: UserId,
      time: Instant,
      cp: CurrencyPair,
      condition: Condition,
      triggeredBy: Indicator = Indicators.trendChangeDetection
  ): Signal =
    Signal(
      userId = uid,
      currencyPair = cp,
      interval = Interval.H1,
      time = time,
      condition = condition,
      triggeredBy = triggeredBy
    )

  def trend(direction: Direction, uid: UserId = Users.uid, time: Instant = ts, cp: CurrencyPair = Markets.gbpeur): Signal =
    make(
      uid = uid,
      time = time,
      cp = cp,
      condition = Condition.TrendDirectionChange(Direction.Downward, direction, Some(1)),
      triggeredBy = Indicators.trendChangeDetection
    )

  def crossover(direction: Direction, uid: UserId = Users.uid, time: Instant = ts, cp: CurrencyPair = Markets.gbpeur): Signal =
    make(
      uid = uid,
      time = time,
      cp = cp,
      condition = Condition.LinesCrossing(direction),
      triggeredBy = Indicators.linesCrossing
    )

  def composite(
      conditions: List[Condition],
      triggers: List[Indicator],
      uid: UserId = Users.uid,
      time: Instant = ts,
      cp: CurrencyPair = Markets.gbpeur
  ): Signal =
    make(
      uid = uid,
      time = time,
      cp = cp,
      condition = Condition.Composite(NonEmptyList.fromListUnsafe(conditions)),
      triggeredBy = Indicator.Composite(NonEmptyList.fromListUnsafe(triggers))
    )

  def aboveThreshold(
      threshold: BigDecimal = BigDecimal(80),
      value: BigDecimal = BigDecimal(85),
      uid: UserId = Users.uid,
      time: Instant = ts,
      cp: CurrencyPair = Markets.gbpeur
  ): Signal =
    make(
      uid = uid,
      time = time,
      cp = cp,
      condition = Condition.ThresholdCrossing(threshold, value, Direction.Upward, Boundary.Upper),
      triggeredBy = Indicators.trendChangeDetection
    )

  def belowThreshold(
      threshold: BigDecimal = BigDecimal(20),
      value: BigDecimal = BigDecimal(15),
      uid: UserId = Users.uid,
      time: Instant = ts,
      cp: CurrencyPair = Markets.gbpeur
  ): Signal =
    make(
      uid = uid,
      time = time,
      cp = cp,
      condition = Condition.ThresholdCrossing(threshold, value, Direction.Downward, Boundary.Lower),
      triggeredBy = Indicators.trendChangeDetection
    )
}
