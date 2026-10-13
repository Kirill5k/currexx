package currexx.backtest.optimizer

/** Selects the parameters that can change within one population. */
enum OptimisationTarget:
  case Indicator, Strategy, Both
