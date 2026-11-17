package currexx.core.fixtures

import cats.effect.IO
import currexx.core.settings.{GlobalSettings, SignalSettings, StrategyConfiguration, StrategyRuntime}
import currexx.core.signal.{Signal, SignalBatch}
import currexx.core.trade.db.TradeSettingsRepository
import currexx.domain.market.Interval
import currexx.domain.signal.{Indicator, ValueRole, ValueSource, ValueTransformation}
import currexx.domain.user.UserId

object Processing:
  val indicators: List[Indicator] = List(
    Indicators.trendChangeDetection,
    Indicators.linesCrossing,
    Indicator.ThresholdCrossing(ValueSource.Close, ValueTransformation.RSX(14), 80, 20),
    Indicator.BollingerBands(ValueSource.Close, ValueTransformation.SMA(20), 20, 2),
    Indicator.VolatilityRegimeDetection(14, ValueTransformation.SMA(30))
  ) ++ ValueRole.values.toList.map(role =>
    Indicator.ValueTracking(
      role,
      ValueSource.Close,
      role match
        case ValueRole.Volatility => ValueTransformation.ATR(14)
        case ValueRole.Momentum   => ValueTransformation.RSX(14)
        case _                    => ValueTransformation.SMA(1)
    )
  ) ++
    ValueRole.values.toList.map(role => Indicator.PriceLineCrossing(ValueSource.Close, role, ValueTransformation.SMA(20)))
  val sources = indicators.flatMap(i => i.resolvedId.map(_ -> i)).toMap

  def runtime(configuration: UserId => IO[StrategyConfiguration]): StrategyRuntime[IO] =
    new StrategyRuntime[IO]:
      def load(uid: UserId): IO[StrategyConfiguration] = configuration(uid)

  val fixed: StrategyRuntime[IO] =
    runtime(_ => IO.pure(StrategyConfiguration(0L, sources, Some(SignalSettings(indicators)), Some(Settings.trade))))
  def signals(repo: UserId => IO[SignalSettings]): StrategyRuntime[IO] =
    runtime(uid => repo(uid).flatMap(s => IO.fromEither(StrategyConfiguration.from(GlobalSettings(uid, Some(s), None, None)))))
  def trades(repo: TradeSettingsRepository[IO]): StrategyRuntime[IO] =
    runtime(uid => repo.get(uid).map(t => StrategyConfiguration(0L, sources, Some(SignalSettings(indicators)), Some(t))))

  def batch(signals: List[Signal]): SignalBatch = SignalBatch(
    interval = signals.headOption.fold(Interval.H1)(_.interval),
    revision = 0L,
    candleTime = signals.headOption.fold(Markets.ts)(_.time),
    closureGap = None,
    sources = sources,
    signals = signals
  )
