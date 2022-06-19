package currexx.algorithms.operators

import cats.Id
import cats.effect.Sync
import cats.syntax.option.*
import cats.syntax.functor.*
import cats.syntax.flatMap.*

import scala.reflect.ClassTag
import scala.util.Random

trait Crossover[F[_], I]:
  def cross(par1: I, par2: I)(using r: Random): F[I]
  def cross(par1: I, par2: I, crossoverProbability: Double)(using r: Random): F[Option[I]]
  protected def maybeCrossSync(par1: I, par2: I, crossoverProbability: Double)(using r: Random, F: Sync[F]): F[Option[I]] =
    F.delay(r.nextDouble() < crossoverProbability)
      .flatMap {
        case true  => cross(par1, par2).map(_.some)
        case false => F.pure(None)
      }

object Crossover:
  def pureThreeWaySplit[G: ClassTag] = new Crossover[Id, Array[G]] {
    override def cross(par1: Array[G], par2: Array[G])(using r: Random): Id[Array[G]] =
      val child  = Array.ofDim[G](par1.length)
      val middle = par1.length / 2
      val point1 = r.nextInt(middle)
      val point2 = r.nextInt(middle) + middle
      var i      = 0
      while (i < point1) {
        child(i) = par1(i)
        i += 1
      }
      while (i < point2) {
        child(i) = par2(i)
        i += 1
      }
      while (i < par1.length) {
        child(i) = par1(i)
        i += 1
      }
      child

    override def cross(par1: Array[G], par2: Array[G], crossoverProbability: Double)(using r: Random): Id[Option[Array[G]]] =
      Option.when(r.nextDouble() < crossoverProbability)(cross(par1, par2))
  }

  def threeWaySplit[F[_], G: ClassTag](using F: Sync[F]): F[Crossover[F, Array[G]]] =
    F.pure {
      val crossover = pureThreeWaySplit[G]
      new Crossover[F, Array[G]] {
        override def cross(par1: Array[G], par2: Array[G])(using r: Random): F[Array[G]] =
          F.delay(crossover.cross(par1, par2))
        override def cross(par1: Array[G], par2: Array[G], crossoverProbability: Double)(using r: Random): F[Option[Array[G]]] =
          F.delay(crossover.cross(par1, par2, crossoverProbability))
      }
    }
