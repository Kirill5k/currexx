package currexx.core.market.db

import currexx.core.market.{IndicatorState, MarketState, PositionState}
import currexx.domain.market.{CurrencyPair, IndicatorKind}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class MarketStateEntity(
    _id: ObjectId,
    userId: ObjectId,
    currencyPair: CurrencyPair,
    currentPosition: Option[PositionState],
    signals: Map[IndicatorKind, List[IndicatorState]],
    lastUpdatedAt: Option[Instant],
    createdAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: MarketState = MarketState(UserId(userId), currencyPair, currentPosition, signals, lastUpdatedAt, createdAt)

object MarketStateEntity:
  def make(
      userId: UserId,
      currencyPair: CurrencyPair,
      currentPosition: Option[PositionState] = None,
      signals: Map[IndicatorKind, List[IndicatorState]] = Map.empty,
      lastUpdatedAt: Option[Instant] = None,
      createdAt: Option[Instant] = Some(Instant.now())
  ): MarketStateEntity =
    MarketStateEntity(ObjectId(), userId.toObjectId, currencyPair, currentPosition, signals, lastUpdatedAt, createdAt)
