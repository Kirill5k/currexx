package currexx.backtest.walkforward

import currexx.backtest.MarketDataProvider.DateRange

import java.nio.ByteBuffer
import java.nio.charset.StandardCharsets.UTF_8
import java.security.MessageDigest

final case class WalkForwardWindow(index: Int, trainingFolds: List[DateRange], selection: DateRange, test: DateRange):
  def training: DateRange = DateRange(trainingFolds.head.from, trainingFolds.last.until)

  /** Stable across experiment IDs and scheduling order; no shared random generator is consumed. */
  def seed(masterSeed: Long): Long =
    def identity(range: DateRange): String = s"${range.from}/${range.until}"
    val key                                =
      s"${WalkForwardWindow.seedVersion}|$masterSeed|${trainingFolds.map(identity).mkString(";")}|${identity(selection)}|${identity(test)}"
    ByteBuffer.wrap(MessageDigest.getInstance("SHA-256").digest(key.getBytes(UTF_8))).getLong

object WalkForwardWindow:
  val seedVersion: String = "walk-forward-v2"
