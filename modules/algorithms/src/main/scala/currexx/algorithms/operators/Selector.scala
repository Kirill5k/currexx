package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync
import currexx.algorithms.collections.*
import currexx.algorithms.{DistributedPopulation, EvaluatedPopulation, Fitness}

import scala.util.Random

trait Selector[F[_], I]:
  def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]]

object Selector:
  def pureRouletteWheel[I] = new Selector[Id, I] {
    override def selectPairs(popByFitness: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] = {
      val fitnessSum = popByFitness.map(_._2).reduce(_ + _)
      val cumulativeFitness = popByFitness.scanLeft(BigDecimal(0.0))((acc, i) => acc + i._2.value).tail

      List
        .fill(populationLimit)(selectOne(popByFitness, fitnessSum, cumulativeFitness))
        .toVector
        .pairs
    }

    private def selectOne(
        population: EvaluatedPopulation[I],
        fitnessSum: Fitness,
        cumulativeFitness: Vector[BigDecimal]
    )(using r: Random): I = {
      val random = r.nextDouble() * fitnessSum.value
      val index = cumulativeFitness.indexWhere(_ >= random)
      if (index == -1) population.last._1 else population(index)._1
    }
  }

  def rouletteWheel[F[_], I](using F: Sync[F]): F[Selector[F, I]] =
    F.pure {
      new Selector[F, I] {
        val rouletteWheelSelector = pureRouletteWheel[I]
        override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]] =
          F.delay(rouletteWheelSelector.selectPairs(population, populationLimit))
      }
    }


  def pureTournament[I] = new Selector[Id, I] {
    override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): Id[DistributedPopulation[I]] =
      List
        .fill(populationLimit)(selectOne(population))
        .toVector
        .pairs

    private def selectOne(population: EvaluatedPopulation[I])(using r: Random): I = {
      val p1 = population(r.nextInt(population.length))
      val p2 = population(r.nextInt(population.length))
      if (p1._2 > p2._2) p1._1 else p2._1
    }
  }

  def tournament[F[_], I](using F: Sync[F]): F[Selector[F, I]] =
    F.pure {
      new Selector[F, I] {
        val tournamentSelector = pureTournament[I]
        override def selectPairs(population: EvaluatedPopulation[I], populationLimit: Int)(using r: Random): F[DistributedPopulation[I]] =
          F.delay(tournamentSelector.selectPairs(population, populationLimit))
      }
    }