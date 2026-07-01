package currexx.backtest

import cats.effect.{IO, IOApp}
import currexx.backtest.services.TestServices
import currexx.backtest.syntax.*
import currexx.core.signal.SignalDetector
import fs2.Stream
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object BatchBacktester extends IOApp.Simple {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  val strategies: List[(String, TestStrategy)] = List(
    "s1"              -> TestStrategy.s1,
    "s1_v2"           -> TestStrategy.s1_v2,
    "s1_v2_optimized" -> TestStrategy.s1_v2_optimized,
    "s2"              -> TestStrategy.s2,
    "s2_v2"           -> TestStrategy.s2_v2,
    "s3"              -> TestStrategy.s3,
    "s4"              -> TestStrategy.s4,
    "s4_optimized"    -> TestStrategy.s4_optimized,
    "s5"              -> TestStrategy.s5,
    "s6"              -> TestStrategy.s6,
    "s6_optimized"    -> TestStrategy.s6_optimized,
    "s7"              -> TestStrategy.s7,
    "s7_optimized"    -> TestStrategy.s7_optimized,
    "s8"              -> TestStrategy.s8,
    "s8_optimized"    -> TestStrategy.s8_optimized,
    "s9"              -> TestStrategy.s9,
    "s9_optimized"    -> TestStrategy.s9_optimized,
    "s10"             -> TestStrategy.s10,
    "s10_optimized"   -> TestStrategy.s10_optimized,
    "s11"             -> TestStrategy.s11,
    "s11_optimized"   -> TestStrategy.s11_optimized,
    "s12"             -> TestStrategy.s12
  )

  def runOne(name: String, ts: TestStrategy): IO[String] =
    Stream
      .emits(MarketDataProvider.majors1h)
      .parEvalMap(6) { filePath =>
        val cp       = MarketDataProvider.cpFromFilePath(filePath)
        val settings = TestSettings.make(cp, ts.rules, List(ts.indicator))
        for
          services <- TestServices.make[IO](settings)
          _        <- MarketDataProvider
            .read[IO](filePath)
            .through(services.processMarketData(SignalDetector.pure))
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
        yield orderStats
      }
      .compile
      .toList
      .map { stats =>
        val profit  = stats.map(_.totalProfit).sum
        val wl      = stats.map(_.winLossRatio).median
        val orders  = stats.map(_.total).sum
        val buys    = stats.map(_.buys).sum
        val sells   = stats.map(_.sells).sum
        val medProf = stats.map(_.totalProfit).median
        f"$name%-16s profit=${profit}%9.5f  W/L=${wl}%8.4f  orders=${orders}%5d  buys=${buys}%5d  sells=${sells}%5d  medProfit=${medProf}%8.5f"
      }

  override val run: IO[Unit] =
    strategies
      .foldLeft(IO.pure(List.empty[String])) { (acc, kv) =>
        acc.flatMap(lines => runOne(kv._1, kv._2).map(l => lines :+ l))
      }
      .flatMap(lines => IO.println("\n===== BATCH RESULTS =====\n" + lines.mkString("\n")))
}
