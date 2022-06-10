package currexx.algorithms.operators

import scala.reflect.ClassTag
import scala.util.Random

trait Crossover[I]:
  def cross(par1: I, par2: I)(using r: Random): I
  def cross(par1: I, par2: I, crossoverProbability: Double)(using r: Random): Option[I] =
    Option.when(r.nextDouble() < crossoverProbability)(cross(par1, par2))

object Crossover:
  inline def threeWaySplit[G: ClassTag]: Crossover[Array[G]] = new Crossover[Array[G]] {
    override def cross(par1: Array[G], par2: Array[G])(using r: Random): Array[G] = {
      val middle      = par1.length / 2
      val point1: Int = r.nextInt(middle)
      val point2: Int = r.nextInt(middle) + middle
      val left        = par1.slice(0, point1)
      val mid         = par1.slice(point1, point2).toSet
      val right       = par1.slice(point2, par1.length)
      left ++ par2.filter(mid.contains) ++ right
    }
  }