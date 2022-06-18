package currexx.backtest

import cats.Show
import cats.effect.{IO, IOApp}
import cats.syntax.traverse.*
import cats.syntax.option.*
import currexx.algorithms.{Fitness, Parameters}
import currexx.algorithms.operators.{Crossover, Elitism, Evaluator, Initialiser, Mutator, Selector}
import currexx.backtest.optimizer.{Optimisable, OptimisationAlgorithm}
import currexx.backtest.optimizer.Optimisable.given
import currexx.backtest.services.TestServices
import currexx.domain.market.{CurrencyPair, Indicator, MovingAverage, ValueSource, ValueTransformation}
import squants.market.{EUR, GBP}
import fs2.Stream

import scala.util.Random

object Optimizer extends IOApp.Simple {

  extension [A](arr: Array[A])
    def withAddedElement(i: Int, a: A): Array[A] =
      arr.update(i, a)
      arr

  given Random = Random()

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
    val cp = CurrencyPair(EUR, GBP)
    for
      testData <- MarketDataProvider.read[IO](testFilePath, cp).compile.toList
      eval <- Evaluator.cached[IO, Array[Array[Int]]] { individual =>
        val settings = TestSettings.make(cp, Indicator.TrendChangeDetection(ValueSource.Close, opt.fromGenome(individual)))
        for
          services <- TestServices.make[IO](settings)
          _ <- Stream
            .emits(testData)
            .through(services.processMarketData)
            .compile
            .drain
          orderStats <- services.getAllOrders.map(OrderStatsCollector.collect)
        yield (individual, Fitness(orderStats.totalProfit))
      }
    yield eval

  def mutator: IO[Mutator[IO, Array[Array[Int]]]] = Mutator.bitFlip[IO].map { bitFlipMutator =>
    new Mutator[IO, Array[Array[Int]]] {
      override def mutate(ind: Array[Array[Int]], mutationProbability: Double)(using r: Random): IO[Array[Array[Int]]] =
        ind
          .foldLeft((IO.pure(Array.ofDim[Array[Int]](ind.length)), 0)) { case ((res, i), gene) =>
            val child = if (gene.length == 1) IO.pure(gene) else bitFlipMutator.mutate(gene, mutationProbability)
            (child.flatMap(compRes => res.map(_.withAddedElement(i, compRes))), i + 1)
          }
          ._1
    }
  }

  def crossover: IO[Crossover[IO, Array[Array[Int]]]] = Crossover.threeWaySplit[IO, Int].map { threeWaySplitCrossover =>
    new Crossover[IO, Array[Array[Int]]] {
      override def cross(par1: Array[Array[Int]], par2: Array[Array[Int]])(using r: Random): IO[Array[Array[Int]]] =
        println(s"Crossing ${par1.map(_.length).mkString(" ")} and ${par2.map(_.length).mkString(" ")}")
        par1
          .foldLeft((IO.pure(Array.ofDim[Array[Int]](par1.length)), 0)) { case ((res, i), g1) =>
            val child = if (g1.length == 1) IO.pure(g1) else threeWaySplitCrossover.cross(g1, par2(i))
            (child.flatMap(compRes => res.map(_.withAddedElement(i, compRes))), i + 1)
          }
          ._1

      override def cross(
          par1: Array[Array[Int]],
          par2: Array[Array[Int]],
          crossoverProbability: Double
      )(using
          r: Random
      ): IO[Option[Array[Array[Int]]]] =
        maybeCrossSync(par1, par2, crossoverProbability)
    }
  }

  val gaParameters = Parameters.GA(
    populationSize = 100,
    maxGen = 200,
    crossoverProbability = 0.7,
    mutationProbability = 0.2,
    elitismRatio = 0.4,
    shuffle = true
  )

  val target = ValueTransformation.sequenced(
    ValueTransformation.NMA(37, 4, 8.0d, MovingAverage.Weighted)
  )

  override def run: IO[Unit] =
    for
      init  <- initialiser
      cross <- crossover
      mut   <- mutator
      eval  <- evaluator("usd-pln-1d.csv")
      sel   <- Selector.rouletteWheel[IO, Array[Array[Int]]]
      elit  <- Elitism.simple[IO, Array[Array[Int]]]
      res   <- OptimisationAlgorithm.ga[IO](init, cross, mut, eval, sel, elit).optimise(target, gaParameters)
      _     <- IO.println(s"${res._1} - ${res._2}")
    yield ()
}
