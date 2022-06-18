package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import cats.syntax.option.*
import currexx.algorithms.Fitness
import currexx.algorithms.operators.{Crossover, Evaluator, Initialiser, Mutator}
import currexx.backtest.Backtester.settings
import currexx.backtest.optimizer.Optimisable
import currexx.backtest.services.TestServices
import currexx.domain.market.{Indicator, MovingAverage, ValueSource, ValueTransformation}

import scala.util.Random

object Optimizer extends IOApp.Simple {

  given showArray[A: Show]: Show[Array[A]] = new Show[Array[A]]:
    override def show(t: Array[A]): String = t.map(Show[A].show).mkString(",")

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

  def mutator: IO[Mutator[IO, Array[Array[Int]]]] = Mutator.bitFlip[IO].map { bitFlipMutator =>
    new Mutator[IO, Array[Array[Int]]] {
      override def mutate(ind: Array[Array[Int]], mutationProbability: Double)(using r: Random): IO[Array[Array[Int]]] =
        ind.toList
          .traverse { gene =>
            if (gene.size == 1) IO.pure(gene)
            else bitFlipMutator.mutate(gene, mutationProbability)
          }
          .map(_.toArray)
    }
  }

  def crossover: IO[Crossover[IO, Array[Array[Int]]]] = Crossover.threeWaySplit[IO, Int].map { threeWaySplitCrossover =>
    new Crossover[IO, Array[Array[Int]]] {
      override def cross(par1: Array[Array[Int]], par2: Array[Array[Int]])(using r: Random): IO[Array[Array[Int]]] =
        par1.toList.zip(par2.toList)
          .traverse { (gene1, gene2) =>
            if (gene1.size == 1) IO.pure(gene1)
            else threeWaySplitCrossover.cross(gene1, gene2)
          }
          .map(_.toArray)

      override def cross(par1: Array[Array[Int]], par2: Array[Array[Int]], crossoverProbability: Double)(using r: Random): IO[Option[Array[Array[Int]]]] =
        maybeCrossSync(par1, par2, crossoverProbability)
    }

  }

  override def run: IO[Unit] = IO.unit
}
