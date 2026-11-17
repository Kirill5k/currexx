package currexx.core.market

import currexx.domain.signal.{Boundary, Direction, Indicator, IndicatorId, ValueRole, VolatilityRegime}
import io.circe.{Codec, Decoder, Encoder, JsonObject}
import io.circe.derivation.{Configuration, ConfiguredCodec}
import io.circe.syntax.*
import kirill5k.common.syntax.time.*

import LegacyMarketProfile.*
import java.time.Instant
import scala.concurrent.duration.FiniteDuration

enum Reading derives ConfiguredCodec:
  case Trend(direction: Direction, confirmedAt: Instant)
  case Zone(zone: MomentumZone, confirmedAt: Instant)
  case Regime(regime: VolatilityRegime, confirmedAt: Instant)
  case Crossing(direction: Direction, confirmedAt: Instant)
  case BandCrossing(boundary: Boundary, direction: Direction, confirmedAt: Instant)
  case Value(value: BigDecimal)

  def adjustForMarketClosure(gap: FiniteDuration, reopenedAt: Instant): Option[Reading] =
    def shift(at: Instant): Instant = if at.isBefore(reopenedAt) then at.plus(gap) else at
    this match
      case Trend(d, at)           => Some(Trend(d, shift(at)))
      case Zone(z, at)            => Some(Zone(z, shift(at)))
      case Regime(r, at)          => Some(Regime(r, shift(at)))
      case Crossing(_, at)        => Option.when(!at.isBefore(reopenedAt))(this)
      case BandCrossing(_, _, at) => Option.when(!at.isBefore(reopenedAt))(this)
      case _: Value               => Some(this)

object Reading:
  given Configuration = Configuration.default.withDiscriminator("kind").withKebabCaseConstructorNames

final case class MarketProfile(readings: Map[IndicatorId, Reading] = Map.empty):
  def trend(id: IndicatorId = IndicatorId.Trend): Option[Reading.Trend]            = readings.get(id).collect { case r: Reading.Trend => r }
  def crossover(id: IndicatorId = IndicatorId.Crossover): Option[Reading.Crossing] =
    readings.get(id).collect { case r: Reading.Crossing => r }
  def momentum(id: IndicatorId = IndicatorId.MomentumZone): Option[Reading.Zone] = readings.get(id).collect { case r: Reading.Zone => r }
  def volatility(id: IndicatorId = IndicatorId.VolatilityRegime): Option[Reading.Regime] =
    readings.get(id).collect { case r: Reading.Regime => r }
  def band(id: IndicatorId = IndicatorId.Band): Option[Reading.BandCrossing] =
    readings.get(id).collect { case r: Reading.BandCrossing => r }
  def number(id: IndicatorId): Option[BigDecimal]               = readings.get(id).collect { case Reading.Value(v) => v }
  def updated(id: IndicatorId, reading: Reading): MarketProfile = copy(readings = readings.updated(id, reading))

  def adjustForMarketClosure(gap: FiniteDuration, reopenedAt: Instant): MarketProfile =
    copy(readings = readings.flatMap { (id, value) =>
      value.adjustForMarketClosure(gap, reopenedAt).map(id -> _)
    })

object MarketProfile:
  def legacyCompatible(sources: Map[IndicatorId, Indicator]): Boolean =
    sources.values.count(_.definition.isInstanceOf[Indicator.PriceLineCrossing]) <= 1 &&
      sources.forall((id, i) => i.explicitId.isEmpty && IndicatorId.defaultFor(i).contains(id))

  given Codec.AsObject[MarketProfile] = Codec.AsObject.from(
    Decoder.instance { cursor =>
      if cursor.downField("readings").succeeded then cursor.get[Map[IndicatorId, Reading]]("readings").map(MarketProfile(_))
      else cursor.as[LegacyMarketProfile].map(fromLegacy)
    },
    Encoder.AsObject.instance(profile => encode(profile, mirrorLegacy = false))
  )

  def encode(profile: MarketProfile, mirrorLegacy: Boolean): JsonObject =
    val current = JsonObject("readings" -> profile.readings.asJson)
    if mirrorLegacy then current.deepMerge(toLegacy(profile).asJsonObject) else current

  def fromLegacy(old: LegacyMarketProfile): MarketProfile =
    val p      = MarketProfile()
    val states = List(
      old.trend.map(s => IndicatorId.Trend -> Reading.Trend(s.direction, s.confirmedAt)),
      old.crossover.map(s => IndicatorId.Crossover -> Reading.Crossing(s.direction, s.confirmedAt)),
      old.momentum.map(s => IndicatorId.MomentumZone -> Reading.Zone(s.zone, s.confirmedAt)),
      old.volatility.map(s => IndicatorId.VolatilityRegime -> Reading.Regime(s.regime, s.confirmedAt)),
      old.lastBandCrossing.map(s => IndicatorId.Band -> Reading.BandCrossing(s.boundary, s.direction, s.confirmedAt)),
      old.lastPriceLineCrossing.map(s => IndicatorId.forLine(s.role) -> Reading.Crossing(s.direction, s.confirmedAt))
    ).flatten
    val values = List(
      ValueRole.Momentum          -> old.lastMomentumValue,
      ValueRole.Volatility        -> old.lastVolatilityValue,
      ValueRole.Velocity          -> old.lastVelocityValue,
      ValueRole.ChannelMiddleBand -> old.lastChannelMiddleBandValue,
      ValueRole.TrendStrength     -> old.lastTrendStrengthValue,
      ValueRole.Price             -> old.lastClosePrice
    ).flatMap((role, value) => value.map(v => IndicatorId.forValue(role) -> Reading.Value(v)))
    (states ++ values).foldLeft(p) { case (profile, (id, reading)) => profile.updated(id, reading) }

  def toLegacy(p: MarketProfile): LegacyMarketProfile = LegacyMarketProfile(
    trend = p.trend().map(s => TrendState(s.direction, s.confirmedAt)),
    crossover = p.crossover().map(s => CrossoverState(s.direction, s.confirmedAt)),
    momentum = p.momentum().map(s => MomentumState(s.zone, s.confirmedAt)),
    volatility = p.volatility().map(s => VolatilityState(s.regime, s.confirmedAt)),
    lastMomentumValue = p.number(IndicatorId.forValue(ValueRole.Momentum)),
    lastVolatilityValue = p.number(IndicatorId.forValue(ValueRole.Volatility)),
    lastVelocityValue = p.number(IndicatorId.forValue(ValueRole.Velocity)),
    lastChannelMiddleBandValue = p.number(IndicatorId.forValue(ValueRole.ChannelMiddleBand)),
    lastTrendStrengthValue = p.number(IndicatorId.forValue(ValueRole.TrendStrength)),
    lastClosePrice = p.number(IndicatorId.forValue(ValueRole.Price)),
    lastBandCrossing = p.band().map(s => BandCrossingState(s.boundary, s.direction, s.confirmedAt)),
    lastPriceLineCrossing = ValueRole.values.toList
      .flatMap(role => p.crossover(IndicatorId.forLine(role)).map(s => PriceLineCrossingState(role, s.direction, s.confirmedAt)))
      .sortBy(_.confirmedAt)
      .lastOption
  )
