package currexx.backtest.services

import cats.effect.Temporal
import cats.syntax.flatMap.*
import currexx.backtest.{OrderStats, RiskSettings}
import currexx.core.signal.SignalDetector
import currexx.domain.market.{CurrencyPair, MarketTimeSeriesData}
import fs2.{Pull, Stream}

/** Executes one already prepared pair series without resetting at physical CSV boundaries. */
object PeriodSimulation:
  def run[F[_]: Temporal](
      services: TestServices[F],
      data: List[MarketTimeSeriesData],
      signalDetector: SignalDetector = SignalDetector.pure,
      riskSettings: RiskSettings = RiskSettings()
  ): F[OrderStats] =
    def loop(previous: MarketTimeSeriesData, remaining: List[MarketTimeSeriesData]): F[OrderStats] =
      remaining match
        case current :: rest =>
          services.processBar(previous, ExecutionInput.from(current), signalDetector).flatMap(_ => loop(current, rest))
        case Nil => services.getOrderStats(riskSettings)

    data match
      case first :: (rest @ _ :: _) => Temporal[F].unit.flatMap(_ => loop(first, rest))
      case _ => Temporal[F].raiseError(new IllegalArgumentException("Insufficient market-data windows for priming and execution"))

  final class Prepared private[PeriodSimulation] (
      val currencyPair: CurrencyPair,
      private[services] val steps: List[(MarketTimeSeriesData, ExecutionInput)]
  )

  private[backtest] def prepare(data: List[MarketTimeSeriesData]): Either[IllegalArgumentException, Prepared] =
    data match
      case first :: (rest @ _ :: _) =>
        val steps = data.lazyZip(rest).map((signal, current) => signal -> ExecutionInput.from(current))
        Right(new Prepared(first.currencyPair, steps))
      case _ => Left(new IllegalArgumentException("Insufficient market-data windows for priming and execution"))

  private[backtest] def runPrepared[F[_]: Temporal](
      services: TestServices[F],
      period: Prepared,
      signalDetector: SignalDetector
  ): F[OrderStats] =
    def loop(steps: List[(MarketTimeSeriesData, ExecutionInput)]): F[OrderStats] =
      steps match
        case (signal, current) :: rest => services.processBar(signal, current, signalDetector).flatMap(_ => loop(rest))
        case Nil                       => services.getOrderStats()

    loop(period.steps)

  private[backtest] def runStream[F[_]: Temporal](
      services: TestServices[F],
      data: Stream[F, MarketTimeSeriesData],
      signalDetector: SignalDetector,
      riskSettings: RiskSettings
  ): F[OrderStats] =
    data.pull
      .unconsN(2, allowFewer = true)
      .flatMap {
        case Some((initial, remaining)) if initial.size == 2 => Pull.output(initial) >> remaining.pull.echo
        case _ => Pull.raiseError[F](new IllegalArgumentException("Insufficient market-data windows for priming and execution"))
      }
      .stream
      .through(services.processMarketData(signalDetector))
      .compile
      .drain
      .flatMap(_ => services.getOrderStats(riskSettings))
