package currexx.backtest.optimizer

private[optimizer] object Checks {
  def check(condition: Boolean, message: => String): Either[IllegalArgumentException, Unit] =
    Either.cond(condition, (), new IllegalArgumentException(message))

  def probability(value: Double): Either[IllegalArgumentException, Unit] =
    check(value.isFinite && value >= 0 && value <= 1, "Probability must be finite and between zero and one")

  def scale(value: Double): Either[IllegalArgumentException, Unit] =
    check(value.isFinite && value >= 0, "Mutation scale must be finite and nonnegative")
}
