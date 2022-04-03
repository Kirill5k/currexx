package currexx.core.common.action

import currexx.core.signal.Signal

enum Action:
  case SignalSubmitted(signal: Signal)
