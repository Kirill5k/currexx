package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.common.action.ActionDispatcher
import currexx.core.fixtures.{Markets, Users}
import currexx.core.signal.db.{SignalRepository, SignalSettingsRepository}
import currexx.core.FileReader
import kirill5k.common.cats.test.IOWordSpec
import currexx.domain.market.*

class SignalServiceISpec extends IOWordSpec {

  "A SignalService" when {
    "detectTrendChange" should {
      "do some magic" ignore {
        val timeSeriesData = Markets.timeSeriesData.copy(prices = FileReader.pricesFromResources("aud-jpy.json"))
        val indicator = Indicator.TrendChangeDetection(
          ValueSource.Close,
          ValueTransformation.sequenced(
            ValueTransformation.HMA(6),
            ValueTransformation.Kalman(0.5)
          )
        )
        val signal = SignalService.detectTrendChange(Users.uid, timeSeriesData, indicator.asInstanceOf[Indicator.TrendChangeDetection])

        signal mustBe None
      }
    }
  }

  def mocks: (SignalRepository[IO], SignalSettingsRepository[IO], ActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[SignalSettingsRepository[IO]], mock[ActionDispatcher[IO]])

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
