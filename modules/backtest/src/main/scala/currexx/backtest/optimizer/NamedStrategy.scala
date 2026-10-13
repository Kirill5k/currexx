package currexx.backtest.optimizer

import currexx.backtest.TestStrategy

/** A named complete candidate. The round target selects its effective components. */
final case class NamedStrategy(name: String, strategy: TestStrategy)
