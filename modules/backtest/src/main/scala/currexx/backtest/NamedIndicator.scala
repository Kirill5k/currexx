package currexx.backtest

import currexx.domain.signal.Indicator

/** Identifies a parameter seed while keeping its evaluation under the target round's rules. */
final case class NamedIndicator(name: String, indicator: Indicator)
