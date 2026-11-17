package currexx.core.market.db

import currexx.core.market.{MarketProfile, MarketState, PositionState}
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.user.UserId
import io.circe.{Codec, Decoder}
import currexx.domain.signal.{Indicator, IndicatorId}
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
    createdAt: Instant,
    previousProfile: Option[MarketProfile] = None,
    lastCandleTime: Option[Instant] = None,
    lastTimeStateCandle: Option[Instant] = None,
    version: Option[Long] = None,
    configRevision: Option[Long] = None,
    interval: Option[Interval] = None,
    invalidated: Option[Set[IndicatorId]] = None,
    sources: Option[Map[IndicatorId, Indicator]] = None
):
  def toDomain: MarketState = MarketState(
    userId = UserId(userId),
    currencyPair = currencyPair,
    currentPosition = currentPosition,
    profile = profile,
    lastUpdatedAt = lastUpdatedAt,
    createdAt = createdAt,
    previousProfile = previousProfile,
    lastCandleTime = lastCandleTime,
    lastTimeStateCandle = lastTimeStateCandle,
    // Documents written before versioning are stored, so they must never be treated as new.
    version = Some(version.getOrElse(0L)),
    configRevision = configRevision,
    interval = interval,
    invalidated = invalidated.getOrElse(Set.empty),
    sources = sources.getOrElse(Map.empty)
  )

object MarketStateEntity:
  private lazy val derivedCodec           = Codec.AsObject.derived[MarketStateEntity]
  given Codec.AsObject[MarketStateEntity] = Codec.AsObject.from(
    Decoder.instance { cursor =>
      derivedCodec(cursor).map { entity =>
        if entity.configRevision.isEmpty || cursor.downField("profile").downField("readings").succeeded then entity
        else
          entity.copy(
            profile = MarketProfile(),
            previousProfile = None,
            sources = Some(Map.empty),
            invalidated = Some(
              entity.invalidated.getOrElse(Set.empty) ++ entity.profile.readings.keySet ++
                entity.previousProfile.toList.flatMap(_.readings.keySet)
            )
          )
      }
    },
    derivedCodec
  )
