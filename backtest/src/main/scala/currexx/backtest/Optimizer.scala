package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Evaluator, Initialiser}
import currexx.backtest.Backtester.settings
import currexx.backtest.optimizer.Optimisable
import currexx.backtest.services.TestServices
import currexx.domain.market.{Indicator, MovingAverage, ValueSource, ValueTransformation}

import scala.util.Random

object Optimizer extends IOApp.Simple {

  given showArray[A](using S: Show[A]): Show[Array[A]] = new Show[Array[A]]:
    override def show(t: Array[A]): String = t.map(S.show).mkString(",")

  def initialiser(using opt: Optimisable[ValueTransformation], rand: Random): IO[Initialiser[IO, Array[Array[Int]]]] =
    Initialiser.simple[IO, Array[Array[Int]]] { individual =>
      def randomise(transformation: ValueTransformation): ValueTransformation =
        transformation match
          case ValueTransformation.Sequenced(sequence) => ValueTransformation.Sequenced(sequence.map(randomise))
          case ValueTransformation.Kalman(_)           => ValueTransformation.Kalman(rand.nextDouble())
          case ValueTransformation.WMA(_)              => ValueTransformation.WMA(rand.nextInt(23) + 2)
          case ValueTransformation.SMA(_)              => ValueTransformation.SMA(rand.nextInt(23) + 2)
          case ValueTransformation.EMA(_)              => ValueTransformation.EMA(rand.nextInt(23) + 2)
          case ValueTransformation.HMA(_)              => ValueTransformation.HMA(rand.nextInt(23) + 2)
          case ValueTransformation.NMA(_, _, _, _) =>
            ValueTransformation.NMA(rand.nextInt(43) + 2, rand.nextInt(23) + 2, rand.nextInt(61).toDouble, MovingAverage.Weighted)
      IO(randomise(opt.fromGenome(individual))).map(opt.toGenome)
    }

  def evaluator(testFilePath: String)(using opt: Optimisable[ValueTransformation]): IO[Evaluator[IO, Array[Array[Int]]]] =
    Evaluator.cached[IO, Array[Array[Int]]] { individual =>
      val settings = TestSettings.make(Indicator.TrendChangeDetection(ValueSource.Close, opt.fromGenome(individual)))
      for
        services <- TestServices.make[IO](settings)
        _ <- MarketDataProvider
          .read[IO](testFilePath, settings.currencyPair)
          .through(services.processMarketData)
          .compile
          .drain
        orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
      yield (individual, Fitness(orderStats.totalProfit))
    }

  override def run: IO[Unit] = IO.unit
}
