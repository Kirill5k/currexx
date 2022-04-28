package currexx.core.market.db

import currexx.core.market.MarketState
import currexx.domain.market.{CurrencyPair, MarketOrder, PriceRange}
import currexx.domain.user.UserId
import io.circe.Codec
import mongo4cats.bson.ObjectId
import mongo4cats.circe.given

import java.time.Instant

final case class MarketStateEntity(
    _id: ObjectId,
    userId: ObjectId,
    currencyPair: CurrencyPair,
    currentPosition: Option[MarketOrder.Position],
    latestPrice: Option[PriceRange],
    lastUpdatedAt: Option[Instant]
) derives Codec.AsObject:
  def toDomain: MarketState = MarketState(UserId(userId), currencyPair, currentPosition, latestPrice, lastUpdatedAt)

object MarketStateEntity:
  def make(
      userId: UserId,
      currencyPair: CurrencyPair,
      currentPosition: Option[MarketOrder.Position] = None,
      latestPrice: Option[PriceRange] = None,
      lastUpdatedAt: Option[Instant] = None
  ): MarketStateEntity =
    MarketStateEntity(ObjectId(), userId.toObjectId, currencyPair, currentPosition, latestPrice, lastUpdatedAt)
