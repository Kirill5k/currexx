package currexx.algorithms

private[algorithms] object collections:
  extension [A](seq: Vector[A])
    def pairs: Vector[(A, A)] =
      seq.zip(seq.tail).zipWithIndex.collect { case (x, i) if (i + 1) % 2 != 0 => x }
