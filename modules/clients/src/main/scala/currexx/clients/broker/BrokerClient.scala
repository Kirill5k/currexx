package currexx.clients.broker

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import currexx.clients.broker.oanda.OandaBrokerClient
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderExecution, OrderPlacementResult, TradeOrder}

trait BrokerClient[F[_]]:
  def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementResult]
  def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]
  // The fills that opened each trade of the currently open position on the given side, or none if any is unavailable
  def findEntryExecutions(parameters: BrokerParameters, cp: CurrencyPair, position: TradeOrder.Position): F[List[OrderExecution]]

final private class LiveBrokerClient[F[_]](
    private val oandaClient: OandaBrokerClient[F]
) extends BrokerClient[F]:
  override def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    parameters match
      case params: BrokerParameters.Oanda => oandaClient.getCurrentOrders(params, cps)

  override def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementResult] =
    parameters match
      case params: BrokerParameters.Oanda => oandaClient.submit(params, order)

  override def findEntryExecutions(
      parameters: BrokerParameters,
      cp: CurrencyPair,
      position: TradeOrder.Position
  ): F[List[OrderExecution]] =
    parameters match
      case params: BrokerParameters.Oanda => oandaClient.findEntryExecutions(params, cp, position)

object BrokerClient:
  def make[F[_]: Monad](
      oandaClient: OandaBrokerClient[F]
  ): F[BrokerClient[F]] =
    LiveBrokerClient[F](oandaClient).pure[F]
