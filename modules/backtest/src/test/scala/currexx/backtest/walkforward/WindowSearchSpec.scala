package currexx.backtest.walkforward

import cats.effect.IO
import currexx.algorithms.Parameters
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.backtest.MarketDataProvider.{Corpus, Dataset, DateRange}
import currexx.backtest.optimizer.ScoringFunction
import currexx.backtest.optimizer.{OptimisationRound, OptimisationTarget}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Indicator, ValueSource, ValueTransformation as VT}
import fs2.io.file.Files
import kirill5k.common.cats.test.IOWordSpec

import java.time.YearMonth
import java.util.UUID
import scala.concurrent.duration.*

class WindowSearchSpec extends IOWordSpec {
  private val strategy = TestStrategy(
    Indicator.TrendChangeDetection(ValueSource.Close, VT.SMA(10)),
    TradeStrategy(
      List(Rule(TradeAction.OpenLong, Rule.Condition.NoPosition)),
      List(Rule(TradeAction.ClosePosition, Rule.Condition.PositionOpenFor(4.hours)))
    )
  )

  private def dataset(month: Int): Dataset = Dataset(
    "aud-usd-1h-1year-2023-07-2024-06.csv",
    Some(DateRange(YearMonth.of(2023, month), YearMonth.of(2023, month + 1)))
  )

  private val corpus  = Corpus(List(List(dataset(8)), List(dataset(9))), List(dataset(10)))
  private val scoring = new ScoringFunction {
    override def score(stats: List[OrderStats]): Double = 100.0 + stats.map(_.totalProfit.toDouble).sum / 10000.0
    override def violations(stats: List[OrderStats]): List[ScoringFunction.Violation] = Nil
  }
  private val ga = Parameters.GA(4, 1, 0.8, 0.7, 0.25, shuffle = true, initialOversampling = 2)

  "WindowSearch" should {
    val parameters: List[Parameters.GA | Parameters.SCGA] = List(
      ga,
      Parameters.SCGA.from(ga).copy(speciesRadius = 0.1, maxSpecies = 2, interspeciesMatingProbability = 0.2)
    )

    parameters.foreach { params =>
      s"reproduce ${params.name} joint search when the same effect is run after another seed" in {
        val label = s"walk-forward-${params.name.toLowerCase}-Both-${UUID.randomUUID()}"
        val round = OptimisationRound(
          label,
          strategy,
          params,
          scoring,
          corpus,
          shortlistSize = 3,
          target = OptimisationTarget.Both
        )
        val files  = Files.forAsync[IO]
        val result = files.tempDirectory.use { directory =>
          val outputDirectory = directory / "search"
          val search          = WindowSearch.make[IO](poolSize = 2, outputDirectory = outputDirectory)
          val repeated        = search.search(round, seed = 91L)
          for {
            first   <- repeated
            _       <- search.search(round, seed = 117L)
            again   <- repeated
            reports <- files.list(outputDirectory).compile.toList
            content <- files.readUtf8(reports.head).compile.string
          } yield (first, again, outputDirectory, reports, content)
        }

        result.asserting { case (first, again, outputDirectory, reports, content) =>
          reports must not be empty
          reports.foreach { path =>
            path.parent mustBe Some(outputDirectory)
            path.fileName.toString must startWith(s"${params.name.toLowerCase}-optimisation-")
            path.fileName.toString must endWith(s"-$label.md")
          }
          content must include("## Final Results")
          content must include(s"## Search ranking: $label")
          first.finalists must not be empty
          first.finalists.size must be <= 3
          first.finalists.map(_._1) must contain(strategy)
          again mustBe first
          first.finalists.foreach { case (_, training, selection) =>
            training.value must be > 0.0
            selection.value must be > 0.0
          }
          succeed
        }
      }
    }
  }
}
