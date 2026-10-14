package currexx.backtest.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import currexx.backtest.{OrderStats, RiskSettings}
import currexx.core.signal.SignalDetector
import currexx.domain.market.MarketTimeSeriesData
import fs2.Stream

/** Executes one already prepared pair series without resetting at physical CSV boundaries. */
object PeriodSimulation:
  def run[F[_]: Temporal](
      services: TestServices[F],
      data: List[MarketTimeSeriesData],
      signalDetector: SignalDetector = SignalDetector.pure,
      riskSettings: RiskSettings = RiskSettings()
  ): F[OrderStats] =
    runStream(services, Stream.emits(data).covary[F], signalDetector, riskSettings)

  private[backtest] def runStream[F[_]: Temporal](
      services: TestServices[F],
      data: Stream[F, MarketTimeSeriesData],
      signalDetector: SignalDetector,
      riskSettings: RiskSettings
  ): F[OrderStats] =
    data
      .through(services.processMarketData(signalDetector))
      .compile
      .drain
      .flatMap(_ => services.getOrderStats(riskSettings))
