package currexx.algorithms.operators

import cats.effect.Sync
import cats.syntax.option.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*

import scala.reflect.ClassTag
import scala.util.Random

trait Crossover[F[_], I]:
  def cross(par1: I, par2: I)(using r: Random): F[I]
  def cross(par1: I, par2: I, crossoverProbability: Double)(using r: Random): F[Option[I]]

object Crossover:
  inline def threeWaySplit[F[_]: Sync, G: ClassTag]: Crossover[F, Array[G]] = new Crossover[F, Array[G]] {
    override def cross(par1: Array[G], par2: Array[G])(using r: Random): F[Array[G]] =
      Sync[F].delay {
        val middle      = par1.length / 2
        val point1: Int = r.nextInt(middle)
        val point2: Int = r.nextInt(middle) + middle
        val left        = par1.slice(0, point1)
        val mid         = par1.slice(point1, point2).toSet
        val right       = par1.slice(point2, par1.length)
        left ++ par2.filter(mid.contains) ++ right
      }
    override def cross(par1: Array[G], par2: Array[G], crossoverProbability: Double)(using r: Random): F[Option[Array[G]]] =
      Sync[F].ifF(Sync[F].delay(r.nextDouble() < crossoverProbability))(cross(par1, par2).map(Some(_)), Sync[F].pure(None)).flatten
  }