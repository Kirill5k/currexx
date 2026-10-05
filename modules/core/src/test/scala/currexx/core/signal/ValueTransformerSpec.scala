package currexx.core.signal

import cats.data.NonEmptyList
import currexx.core.fixtures.Markets
import currexx.domain.market.PriceRange
import currexx.domain.signal.{ValueSource, ValueTransformation as VT}
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

import java.time.Instant

class ValueTransformerSpec extends AnyWordSpec with Matchers {
  private val transformer = ValueTransformer
  private val data        = Markets.timeSeriesData.copy(prices =
    NonEmptyList.of(
      PriceRange(11.0, 14.0, 8.0, 12.0, 30.0, Instant.EPOCH),
      PriceRange(5.0, 8.0, 2.0, 6.0, 20.0, Instant.EPOCH.minusSeconds(3600)),
      PriceRange(-1.0, 2.0, -4.0, 0.0, 10.0, Instant.EPOCH.minusSeconds(7200))
    )
  )

  "A ValueTransformer" should {
    "extract every source in newest-first order and reuse the window's source buffers" in {
      val window  = new NumericalWindow(data)
      val sources = List(
        ValueSource.Close -> List(12.0, 6.0, 0.0),
        ValueSource.Open  -> List(11.0, 5.0, -1.0),
        ValueSource.HL2   -> List(11.0, 5.0, -1.0),
        ValueSource.HLC3  -> List(34.0 / 3.0, 16.0 / 3.0, -2.0 / 3.0)
      )
      sources.foreach { (source, expected) =>
        val values = transformer.extractFrom(window, source)
        values.toList mustBe expected
        (values eq transformer.extractFrom(window, source)) mustBe true
      }
      window.highs.toList mustBe List(14.0, 8.0, 2.0)
      window.lows.toList mustBe List(8.0, 2.0, -4.0)
      window.volumes.toList mustBe List(30.0, 20.0, 10.0)
      (window.closings eq new NumericalWindow(data).closings) mustBe false
    }

    "calculate native transformations without changing source or OHLC buffers" in {
      val window = new NumericalWindow(data)
      val source = transformer.extractFrom(window, ValueSource.Close)
      val cases  = List(
        VT.SMA(2)               -> List(9.0, 3.0, 0.0),
        VT.EMA(3)               -> List(7.5, 3.0, 0.0),
        VT.WMA(2)               -> List(10.0, 4.0, 0.0),
        VT.StandardDeviation(2) -> List(math.sqrt(18.0), math.sqrt(18.0), 0.0),
        VT.ATR(2)               -> List(7.5, 7.0, 0.0)
      )
      (cases ++ cases.reverse).foreach { (transformation, expected) =>
        withClue(s"$transformation: ") {
          transformer.transformTo(source, window, transformation).toList mustBe expected
          source.toList mustBe List(12.0, 6.0, 0.0)
          window.highs.toList mustBe List(14.0, 8.0, 2.0)
          window.lows.toList mustBe List(8.0, 2.0, -4.0)
        }
      }
    }

    "feed each sequence stage the preceding result across native and List-based kernels" in {
      val window = new NumericalWindow(data)
      val source = window.closings
      val native = VT.Sequenced(List(VT.EMA(3), VT.SMA(2)))
      // With no measurement noise, Kalman follows its supplied observations exactly.
      val mixed = VT.Sequenced(List(VT.EMA(3), VT.Kalman(1.0, 0.0), VT.SMA(2)))
      List(native, mixed, VT.Sequenced(List(VT.Sequenced(List(VT.EMA(3))), VT.SMA(2)))).foreach { transformation =>
        transformer.transformTo(source, window, transformation).toList mustBe List(5.25, 1.5, 0.0)
      }
      transformer.transformTo(source, window, VT.Sequenced(Nil)).toList mustBe List(12.0, 6.0, 0.0)
      source.toList mustBe List(12.0, 6.0, 0.0)
    }

    "use preceding transformed closes with original highs and lows for ATR" in {
      val window = new NumericalWindow(data)
      // EMA(3) closes are [7.5, 3, 0]: the newest true range is 14 - 3 = 11, after a seed ATR of 7.
      transformer.transformTo(window.closings, window, VT.Sequenced(List(VT.EMA(3), VT.ATR(2)))).toList mustBe
        List(9.0, 7.0, 0.0)
      transformer.averageTrueRange(Array(7.5, 3.0, 0.0), window, 2).toList mustBe List(9.0, 7.0, 0.0)
      window.closings.toList mustBe List(12.0, 6.0, 0.0)
    }

    "keep OHLC-based transformations independent of preceding sequence values" in {
      // Midpoint closes give integer typical prices [11, 5, -1] and exact mean deviations of 3 for CCI(2).
      val window = new NumericalWindow(data.copy(prices = data.prices.map(price => price.copy(close = price.open))))
      val input  = Array(-120.0, -60.0, 0.0)
      val cases  = List(
        VT.ADX(1)                        -> List(100.0, 0.0, 0.0),
        VT.WilliamsR(2)                  -> List(-25.0, -25.0, -50.0),
        VT.CCI(2)                        -> List(3.0 / (0.015 * 3.0), 3.0 / (0.015 * 3.0), 0.0),
        VT.IchimokuKijunSen(2)           -> List(8.0, 2.0, -1.0),
        VT.ParabolicSAR(0.02, 0.2, 0.02) -> List(-4.0, -4.0, -4.0),
        VT.CMF(2)                        -> List(0.0, 0.0, 0.0)
      )
      cases.foreach { (transformation, expected) =>
        withClue(s"$transformation after EMA(3): ") {
          val actual = transformer.transformTo(input, window, VT.Sequenced(List(VT.EMA(3), transformation)))
          actual.toList mustBe expected
          input.toList mustBe List(-120.0, -60.0, 0.0)
          window.closings.toList mustBe List(11.0, 5.0, -1.0)
        }
      }
    }
  }
}
