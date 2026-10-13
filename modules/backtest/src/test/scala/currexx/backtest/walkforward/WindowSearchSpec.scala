package currexx.backtest.walkforward

import cats.effect.IO
import currexx.algorithms.Parameters
import currexx.backtest.{OrderStats, TestStrategy}
import currexx.backtest.MarketDataProvider.{Corpus, Dataset, DateRange}
import currexx.backtest.optimizer.ScoringFunction
import currexx.backtest.optimizer.{OptimisationRound, OptimisationTarget}
import currexx.core.trade.{Rule, TradeAction, TradeStrategy}
import currexx.domain.signal.{Direction, Indicator, ValueSource, ValueTransformation as VT}
import fs2.io.file.{Files, Path}
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

  private def deleteReports(label: String): IO[Unit] = {
    val files     = Files.forAsync[IO]
    val directory = Path("optimisation-results")
    files.exists(directory).flatMap { exists =>
      if (exists)
        files
          .list(directory)
          .filter(_.fileName.toString.endsWith(s"-$label.md"))
          .evalMap(files.deleteIfExists)
          .compile
          .drain
      else IO.unit
    }
  }

  "WindowSearch" should {
    val parameters: List[Parameters.GA | Parameters.SCGA] = List(
      ga,
      Parameters.SCGA.from(ga).copy(speciesRadius = 0.1, maxSpecies = 2, interspeciesMatingProbability = 0.2)
    )

    val withoutNumericRules = strategy.copy(rules =
      strategy.rules.copy(
        closeRules = List(Rule(TradeAction.ClosePosition, Rule.Condition.TrendChangedTo(Direction.Downward)))
      )
    )
    val searches = OptimisationTarget.values.toList.map(_ -> strategy) :+ (OptimisationTarget.Both -> withoutNumericRules)

    (for {
      params             <- parameters
      (target, template) <- searches
    } yield (params, target, template)).foreach { case (params, target, template) =>
      val fixedRules    = template == withoutNumericRules
      val configuration = if (fixedRules) "no tunable rule parameters" else "default bounds"
      s"reproduce ${params.name} $target search with $configuration when the same effect is run after another seed" in {
        val label = s"walk-forward-${params.name.toLowerCase}-$target-${UUID.randomUUID()}"
        val round = OptimisationRound(
          label,
          template,
          params,
          scoring,
          corpus,
          shortlistSize = 3,
          target = target
        )
        val search   = WindowSearch.make[IO](poolSize = 2)
        val repeated = search.search(round, seed = 91L)
        val result   = (for {
          first <- repeated
          _     <- search.search(round, seed = 117L)
          again <- repeated
        } yield (first, again)).guarantee(deleteReports(label))

        result.asserting { case (first, again) =>
          first.finalists must not be empty
          first.finalists.size must be <= 3
          first.finalists.map(_._1) must contain(template)
          again mustBe first
          if (fixedRules) FrozenSelection.fromResult(template, first).strategy.rules mustBe template.rules
          else succeed
          first.finalists.foreach { case (candidate, training, selection) =>
            target match
              case OptimisationTarget.Indicator          => candidate.rules mustBe template.rules
              case OptimisationTarget.Strategy           => candidate.indicator mustBe template.indicator
              case OptimisationTarget.Both if fixedRules => candidate.rules mustBe template.rules
              case OptimisationTarget.Both               => succeed
            training.value must be > 0.0
            selection.value must be > 0.0
          }
          succeed
        }
      }
    }
  }
}
