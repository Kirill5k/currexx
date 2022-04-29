package currexx.core.market

import cats.effect.IO
import currexx.core.CatsSpec
import currexx.domain.user.UserId
import currexx.core.common.action.ActionDispatcher
import currexx.core.market.db.MarketStateRepository
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, PriceRange}

class MarketServiceSpec extends CatsSpec {

  "A MarketService" when {
    "getState" should {
      "return state of all traded currencies" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.getAll(any[UserId])).thenReturn(IO.pure(List(Markets.state)))

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.getState(Users.uid)
        yield state

        result.asserting { res =>
          verify(stateRepo).getAll(Users.uid)
          verifyNoInteractions(disp)
          res mustBe List(Markets.state)
        }
      }
    }

    "processMarketData" should {
      "update state with latest received priced" in {
        val (stateRepo, disp) = mocks
        when(stateRepo.update(any[UserId], any[CurrencyPair], any[PriceRange])).thenReturn(IO.unit)

        val result = for
          svc   <- MarketService.make[IO](stateRepo, disp)
          state <- svc.processMarketData(Users.uid, Markets.timeSeriesData)
        yield state

        result.asserting { res =>
          verify(stateRepo).update(Users.uid, Markets.gbpeur, Markets.priceRange)
          verifyNoInteractions(disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MarketStateRepository[IO], ActionDispatcher[IO]) =
    (mock[MarketStateRepository[IO]], mock[ActionDispatcher[IO]])
}
