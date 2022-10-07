package currexx.core.monitor.db

import cats.data.NonEmptyList
import io.circe.{Codec, CursorOp, Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.*
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.core.monitor.{CreateMonitor, Monitor, MonitorId}
import currexx.domain.user.UserId
import currexx.domain.json.given
import currexx.domain.monitor.Schedule
import mongo4cats.bson.{BsonValue, ObjectId}
import mongo4cats.circe.given

import java.time.Instant
import scala.concurrent.duration.FiniteDuration

sealed trait MonitorEntity(val kind: String):
  def _id: ObjectId
  def userId: ObjectId
  def active: Boolean
  def currencyPairs: List[String]
  def schedule: Schedule
  def lastQueriedAt: Option[Instant]
  def toDomain: Monitor

object MonitorEntity {
  final case class Profit(
      _id: ObjectId,
      userId: ObjectId,
      active: Boolean,
      currencyPairs: List[String],
      schedule: Schedule,
      lastQueriedAt: Option[Instant],
      min: Option[BigDecimal],
      max: Option[BigDecimal]
  ) extends MonitorEntity("profit")
      derives Codec.AsObject:
    override def toDomain: Monitor =
      Monitor.Profit(
        MonitorId(_id),
        UserId(userId),
        active,
        NonEmptyList.fromListUnsafe(currencyPairs.flatMap(cp => CurrencyPair.from(cp).toOption)),
        schedule,
        lastQueriedAt,
        min,
        max
      )

  final case class MarketData(
      _id: ObjectId,
      userId: ObjectId,
      active: Boolean,
      currencyPairs: List[String],
      schedule: Schedule,
      lastQueriedAt: Option[Instant],
      interval: Interval
  ) extends MonitorEntity("market-data")
      derives Codec.AsObject:
    override def toDomain: Monitor =
      Monitor.MarketData(
        MonitorId(_id),
        UserId(userId),
        active,
        NonEmptyList.fromListUnsafe(currencyPairs.flatMap(cp => CurrencyPair.from(cp).toOption)),
        schedule,
        lastQueriedAt,
        interval
      )

  def from(create: CreateMonitor): MonitorEntity =
    create match
      case CreateMonitor.MarketData(userId, currencyPairs, schedule, interval) =>
        MonitorEntity.MarketData(ObjectId.gen, userId.toObjectId, true, currencyPairs.toList.map(_.toString), schedule, None, interval)
      case CreateMonitor.Profit(userId, currencyPairs, schedule, min, max) =>
        MonitorEntity.Profit(ObjectId.gen, userId.toObjectId, true, currencyPairs.toList.map(_.toString), schedule, None, min, max)

  inline given Decoder[MonitorEntity] = Decoder.instance { c =>
    c.downField("kind").as[String].flatMap {
      case "market-data" => c.as[MarketData]
      case "profit" => c.as[Profit]
      case kind => Left(DecodingFailure(s"Unexpected monitor kind $kind", List(CursorOp.Field("kind"))))
    }
  }

  inline given Encoder[MonitorEntity] = Encoder.instance {
    case marketData: MarketData => marketData.asJsonObject.add("kind", Json.fromString(marketData.kind)).asJson
    case profit: Profit => profit.asJsonObject.add("kind", Json.fromString(profit.kind)).asJson
  }
}
