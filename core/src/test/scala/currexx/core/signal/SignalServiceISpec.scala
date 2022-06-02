package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.core.{CatsSpec, FileReader}
import currexx.domain.market.*
import currexx.domain.user.UserId
import io.circe.JsonObject

import java.time.LocalDate
import scala.collection.immutable.ListMap

class SignalServiceISpec extends CatsSpec {

  "A SignalService" when {
    "detectTrendChange" should {
      "do some magic" ignore {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = FileReader.pricesFromResources("aud-usd-sell.json"))
        val indicator = Indicator.TrendChangeDetection(ValueSource.Close, ValueTransformation.NMA(9, 3, 8d, MovingAverage.Weighted))
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        signal mustBe None
      }
    }
  }

  def mocks: (SignalRepository[IO], SignalSettingsRepository[IO], ActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[SignalSettingsRepository[IO]], mock[ActionDispatcher[IO]])

  extension[A] (nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
