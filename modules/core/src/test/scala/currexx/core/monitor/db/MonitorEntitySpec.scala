package currexx.core.monitor.db

import currexx.core.fixtures.{Markets, Monitors}
import currexx.core.monitor.Monitor
import currexx.domain.market.{CandleCount, CurrencyPair}
import io.circe.Json
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import mongo4cats.circe.given
import mongo4cats.bson.syntax.*

class MonitorEntitySpec extends AnyWordSpec with Matchers {

  "A MonitorEntity codec" should {

    "convert MonitorEntity to bson" in {
      val count         = CandleCount.from(250).toOption.get
      val monitorEntity = MonitorEntity.from(Monitors.createMarketData(candleCount = count))

      val bson = monitorEntity.toBson
      bson.asDocument.flatMap(_.getAs[List[CurrencyPair]]("currencyPairs")) mustBe Some(List(Markets.gbpeur))
      bson.asDocument.flatMap(_.getAs[Int]("candleCount")) mustBe Some(250)
      monitorEntity.asJson.as[MonitorEntity] mustBe Right(monitorEntity)
      monitorEntity.toDomain.asInstanceOf[Monitor.MarketData].candleCount mustBe count
    }

    "decode legacy entities with a default count and write the count explicitly" in {
      val entity = MonitorEntity.from(Monitors.createMarketData())
      val legacy = entity.asJson.mapObject(_.remove("candleCount"))

      legacy.as[MonitorEntity].map(_.toDomain) mustBe Right(entity.toDomain)
      entity.asJson.mapObject(_.add("candleCount", Json.Null)).as[MonitorEntity].map(_.toDomain) mustBe Right(entity.toDomain)
      entity.asJson.hcursor.get[Int]("candleCount") mustBe Right(100)
      entity.toDomain.asInstanceOf[Monitor.MarketData].candleCount mustBe CandleCount.Default
    }

    "reject invalid supplied counts instead of applying the legacy default" in {
      val entity = MonitorEntity.from(Monitors.createMarketData()).asJson
      List(Json.fromString("100"), Json.fromDoubleOrNull(1.5), Json.fromInt(0), Json.fromInt(5000)).foreach { count =>
        entity.mapObject(_.add("candleCount", count)).as[MonitorEntity].isLeft mustBe true
      }
    }
  }
}
