package currexx.core.settings

import currexx.core.trade.StrategyCheck
import currexx.domain.errors.AppError
import currexx.domain.signal.{Indicator, IndicatorId}

final case class StrategyConfiguration(
    revision: Long,
    sources: Map[IndicatorId, Indicator],
    signal: Option[SignalSettings],
    trade: Option[TradeSettings]
)
object StrategyConfiguration:
  def from(settings: GlobalSettings): Either[AppError, StrategyConfiguration] =
    val indicators = settings.signal.toList.flatMap(_.indicators)
    val strategy   = settings.trade.map(_.strategy)
    StrategyCheck.validate(indicators, strategy).left.map(AppError.FailedValidation(_)).map { sources =>
      StrategyConfiguration(settings.revision, sources, settings.signal, settings.trade)
    }
