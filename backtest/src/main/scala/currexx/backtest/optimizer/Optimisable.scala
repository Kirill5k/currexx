package currexx.backtest.optimizer

trait Optimisable[T]:
  def toGenome(target: T): Array[Array[Int]]
  def fromGenome(genome: Array[Array[Int]]): T
