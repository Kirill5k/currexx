package currexx.core.signal

import cats.data.NonEmptyList
import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.domain.user.UserId
import currexx.domain.market.{Condition, Indicator}
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Signals, Users}
import currexx.core.signal.db.SignalRepository

class SignalServiceSpec extends CatsSpec {

  "A SignalService" when {
    "submit" should {
      "store new signal in the repository and dispatch an action" in {
        val (repo, disp) = mocks
        when(repo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- SignalService.make[IO](repo, disp)
          _   <- svc.submit(Signals.macd)
        yield ()

        result.unsafeToFuture().map { res =>
          verify(repo).save(Signals.macd)
          verify(disp).dispatch(Action.SignalSubmitted(Signals.macd))
          res mustBe ()
        }
      }
    }

    "getAll" should {
      "return all signals from the repository" in {
        val (repo, disp) = mocks
        when(repo.getAll(any[UserId])).thenReturn(IO.pure(List(Signals.macd)))

        val result = for
          svc <- SignalService.make[IO](repo, disp)
          res <- svc.getAll(Users.uid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(disp)
          verify(repo).getAll(Users.uid)
          res mustBe List(Signals.macd)
        }
      }
    }

    "processMarketData" should {
      "not do anything when there are no changes in market data since last point" in {
        val (repo, disp) = mocks

        val result = for
          svc <- SignalService.make[IO](repo, disp)
          res <- svc.processMarketData(Users.uid, Markets.timeSeriesData.copy(prices = Markets.priceRanges))
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(disp, repo)
          res mustBe ()
        }
      }

      "detect MACD line crossing up" in {
        val (repo, disp) = mocks
        when(repo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(2))
        val result = for
          svc <- SignalService.make[IO](repo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.unsafeToFuture().map { res =>
          val expectedSignal = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingUp(), timeSeriesData.prices.head.time)
          verify(repo).save(expectedSignal)
          verify(disp).dispatch(Action.SignalSubmitted(expectedSignal))
          res mustBe ()
        }
      }

      "detect MACD line crossing down" in {
        val (repo, disp) = mocks
        when(repo.save(any[Signal])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val timeSeriesData = Markets.timeSeriesData.copy(prices = Markets.priceRanges.drop(5))
        val result = for
          svc <- SignalService.make[IO](repo, disp)
          res <- svc.processMarketData(Users.uid, timeSeriesData)
        yield res

        result.unsafeToFuture().map { res =>
          val expectedSignal = Signal(Users.uid, Markets.gbpeur, Indicator.MACD, Condition.CrossingDown(), timeSeriesData.prices.head.time)
          verify(repo).save(expectedSignal)
          verify(disp).dispatch(Action.SignalSubmitted(expectedSignal))
          res mustBe ()
        }
      }
    }
  }

  def mocks: (SignalRepository[IO], ActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[ActionDispatcher[IO]])

  extension [A](nel: NonEmptyList[A])
    def drop(n: Int): NonEmptyList[A] =
      NonEmptyList.fromListUnsafe(nel.toList.drop(n))
}
