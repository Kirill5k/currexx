package currexx.clients.broker

import cats.Monad
import cats.data.NonEmptyList
import currexx.clients.broker.oanda.OandaBrokerClient
import currexx.domain.market.{CurrencyPair, OpenedTradeOrder, OrderPlacementStatus, TradeOrder}

trait BrokerClient[F[_]]:
  def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementStatus]
  def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]]

final private class LiveBrokerClient[F[_]](
    private val oandaClient: OandaBrokerClient[F]
) extends BrokerClient[F]:
  override def find(parameters: BrokerParameters, cps: NonEmptyList[CurrencyPair]): F[List[OpenedTradeOrder]] =
    parameters match
      case params: BrokerParameters.Oanda => oandaClient.getCurrentOrders(params, cps)

  override def submit(parameters: BrokerParameters, order: TradeOrder): F[OrderPlacementStatus] =
    parameters match
      case params: BrokerParameters.Oanda => oandaClient.submit(params, order)

object BrokerClient:
  def make[F[_]: Monad](
      oandaClient: OandaBrokerClient[F]
  ): F[BrokerClient[F]] =
    Monad[F].pure(LiveBrokerClient[F](oandaClient))
