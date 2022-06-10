package currexx.algorithms

import scala.reflect.ClassTag
import scala.util.Random

private[algorithms] object collections:
  extension [A](seq: List[A])
    def pairs: List[(A, A)] =
      seq.zip(seq.tail).zipWithIndex collect { case (x, i) if (i + 1) % 2 != 0 => x }
