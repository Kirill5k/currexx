package currexx.core.monitor.db

import currexx.core.fixtures.{Markets, Monitors}
import currexx.domain.market.CurrencyPair
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import mongo4cats.circe.given
import mongo4cats.bson.syntax.*

class MonitorEntitySpec extends AnyWordSpec with Matchers {

  "A MonitorEntity codec" should {

    "convert MonitorEntity to bson" in {
      val monitorEntity = MonitorEntity.from(Monitors.createMarketData())

      val bson = monitorEntity.toBson
      bson.asDocument.flatMap(_.getAs[List[CurrencyPair]]("currencyPairs")) mustBe Some(List(Markets.gbpeur))
    }
  }
}
