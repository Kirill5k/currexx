package currexx.core.market.db

import currexx.core.market.{MarketProfile, MarketState, PositionState}
import currexx.domain.market.CurrencyPair
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
    profile: MarketProfile,
    lastUpdatedAt: Instant,
    createdAt: Instant
) derives Codec.AsObject:
  def toDomain: MarketState = MarketState(
    userId = UserId(userId),
    currencyPair = currencyPair,
    currentPosition = currentPosition,
    profile = profile,
    lastUpdatedAt = lastUpdatedAt,
    createdAt = createdAt
  )

object MarketStateEntity:
  def make(
      userId: UserId,
      currencyPair: CurrencyPair,
      currentPosition: Option[PositionState] = None,
      profile: MarketProfile = MarketProfile(),
      lastUpdatedAt: Instant = Instant.now(),
      createdAt: Instant = Instant.now()
  ): MarketStateEntity =
    MarketStateEntity(
      _id = ObjectId(),
      userId = userId.toObjectId,
      currencyPair = currencyPair,
      currentPosition = currentPosition,
      profile = profile,
      lastUpdatedAt = lastUpdatedAt,
      createdAt = createdAt
    )
