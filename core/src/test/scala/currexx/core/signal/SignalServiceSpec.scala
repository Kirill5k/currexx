package currexx.core.signal

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.core.auth.user.UserId
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Signals, Users}
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
          res   <- svc.getAll(Users.uid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(disp)
          verify(repo).getAll(Users.uid)
          res mustBe List(Signals.macd)
        }
      }
    }
  }

  def mocks: (SignalRepository[IO], ActionDispatcher[IO]) =
    (mock[SignalRepository[IO]], mock[ActionDispatcher[IO]])
}
