package currexx.algorithms

private[algorithms] object collections:
  extension [A](seq: Vector[A])
    def pairs: Vector[(A, A)] =
      if (seq.length < 2) Vector.empty
      else {
        val result = Vector.newBuilder[(A, A)]
        var i = 0
        while (i < seq.length - 1) {
          result += ((seq(i), seq(i + 1)))
          i += 2
        }
        result.result()
      }
