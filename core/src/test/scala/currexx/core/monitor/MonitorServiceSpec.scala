package currexx.core.monitor

import cats.Applicative
import cats.effect.IO
import currexx.core.CatsSpec
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Monitors, Users}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.errors.AppError
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import fs2.Stream
import org.mockito.Mockito

import java.time.Instant
import scala.concurrent.duration.*

class MonitorServiceSpec extends CatsSpec {

  "A MonitorService" when {
    "update" should {
      "update monitor properties in the db" in {
        val (repo, disp) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.update(Monitors.monitor)
        yield res

        result.asserting { res =>
          verifyNoInteractions(disp)
          verify(repo).update(Monitors.monitor)
          res mustBe ()
        }
      }

      "close open orders when currency pair has changed" in {
        val (repo, disp) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val updated = Monitors.monitor.copy(currencyPair = Markets.gbpusd)
        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.update(updated)
        yield res

        result.asserting { res =>
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          verify(repo).update(updated)
          res mustBe ()
        }
      }

      "close open orders when monitor has been deactivated" in {
        val (repo, disp) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val updated = Monitors.monitor.copy(active = false)
        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.update(updated)
        yield res

        result.asserting { res =>
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          verify(repo).update(updated)
          res mustBe ()
        }
      }

      "reschedule profit monitor it it was previously undefined" in {
        val (repo, disp) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor.copy(profit = None)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.update(Monitors.monitor)
        yield res

        result.asserting { res =>
          verify(repo).update(Monitors.monitor)
          verify(disp).dispatch(any[Action.ScheduleProfitMonitor])
          verifyNoMoreInteractions(disp)
          res mustBe ()
        }
      }
    }

    "resume" should {
      "set active to true" in {
        val (repo, disp) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.resume(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(disp)
          verify(repo).activate(Users.uid, Monitors.mid, true)
          res mustBe ()
        }
      }
    }

    "pause" should {
      "set active to false" in {
        val (repo, disp) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.pause(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).activate(Users.uid, Monitors.mid, false)
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          res mustBe ()
        }
      }
    }

    "create" should {
      "store monitor in db and submit query monitor action when schedule is periodic" in {
        val (repo, disp) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          mid <- svc.create(Monitors.create())
        yield mid

        result.asserting { mid =>
          verify(repo).create(Monitors.create())
          verify(disp).dispatch(Action.SchedulePriceMonitor(Users.uid, Monitors.mid, 0.seconds))
          verify(disp).dispatch(Action.ScheduleProfitMonitor(Users.uid, Monitors.mid, 0.seconds))
          mid mustBe Monitors.mid
        }
      }

      "not schedule profit monitor if it is not defined" in {
        val (repo, disp) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          mid <- svc.create(Monitors.create(profit = None))
        yield mid

        result.asserting { mid =>
          verify(repo).create(Monitors.create(profit = None))
          verify(disp).dispatch(Action.SchedulePriceMonitor(Users.uid, Monitors.mid, 0.seconds))
          verifyNoMoreInteractions(disp)
          mid mustBe Monitors.mid
        }
      }

      "store monitor in db and reschedule when schedule is cron" in {
        val (repo, disp) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val priceMonitorSchedule = Monitors.priceMonitorSchedule.copy(schedule = Schedule.Cron("0 7,20 * * 1-5").value)
        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          mid <- svc.create(Monitors.create(price = priceMonitorSchedule, profit = None))
        yield mid

        result.asserting { mid =>
          verify(repo).create(Monitors.create(price = priceMonitorSchedule, profit = None))
          verify(disp).dispatch(any[Action.SchedulePriceMonitor])
          mid mustBe Monitors.mid
        }
      }
    }

    "rescheduleAll" should {
      "schedule monitors without lastQueriedAt to start immediately" in {
        val (repo, disp)   = mocks
        val priceSchedule  = Monitors.priceMonitorSchedule.copy(lastQueriedAt = None)
        val profitSchedule = Monitors.profitMonitorSchedule.copy(lastQueriedAt = None)
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(price = priceSchedule, profit = Some(profitSchedule))))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(Action.SchedulePriceMonitor(Users.uid, Monitors.mid, 0.seconds))
          verify(disp).dispatch(Action.ScheduleProfitMonitor(Users.uid, Monitors.mid, 0.seconds))
          res mustBe ()
        }
      }

      "schedule monitors with old lastQueriedAt to start immediately" in {
        val (repo, disp) = mocks
        val schedule     = Monitors.priceMonitorSchedule.copy(lastQueriedAt = Some(Instant.parse("2020-01-01T00:00:00Z")))
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(price = schedule, profit = None)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(Action.SchedulePriceMonitor(Users.uid, Monitors.mid, 0.seconds))
          verifyNoMoreInteractions(disp)
          res mustBe ()
        }
      }

      "schedule monitors with recent lastQueriedAt to wait" in {
        val (repo, disp) = mocks
        val schedule     = Monitors.priceMonitorSchedule.copy(lastQueriedAt = Some(Instant.now.minusSeconds(50)))
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(price = schedule, profit = None)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(any[Action.SchedulePriceMonitor])
          res mustBe ()
        }
      }
    }

    "triggerProfitMonitor" should {
      "submit assert profit action for active monitor" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor))
        when(repo.updateProfitQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerProfitMonitor(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(repo).updateProfitQueriedTimestamp(Users.uid, Monitors.mid)
          verify(disp, Mockito.times(2)).dispatch(any[Action])
          verifyNoMoreInteractions(repo, disp)
          res mustBe ()
        }
      }

      "return error when profit monitor is not defined" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor.copy(profit = None)))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerProfitMonitor(Users.uid, Monitors.mid)
        yield res

        result.attempt.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verifyNoInteractions(disp)
          res mustBe Left(AppError.NotScheduled("profit"))
        }
      }
    }

    "triggerPriceMonitor" should {
      "submit fetch market data action for active monitor" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor))
        when(repo.updatePriceQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerPriceMonitor(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
//          verify(disp).dispatch(eqTo(Action.ProcessMarketData(Users.uid, Markets.timeSeriesData)))
          verify(repo).updatePriceQueriedTimestamp(Users.uid, Monitors.mid)
          verify(disp, Mockito.times(2)).dispatch(any[Action])
          verifyNoMoreInteractions(repo, disp)
          res mustBe ()
        }
      }

      "not reschedule monitor in case of manual query" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor))
        when(repo.updatePriceQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerPriceMonitor(Users.uid, Monitors.mid, true)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(disp).dispatch(Action.FetchMarketData(Users.uid, Markets.gbpeur, Interval.H1))
          verify(repo).updatePriceQueriedTimestamp(Users.uid, Monitors.mid)
          verifyNoMoreInteractions(repo, disp)
          res mustBe ()
        }
      }

      "not submit fetch market data action for inactive monitor" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor.copy(active = false)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerPriceMonitor(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(disp).dispatch(any[Action.SchedulePriceMonitor])
          verifyNoMoreInteractions(repo, disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MonitorRepository[IO], ActionDispatcher[IO]) =
    (mock[MonitorRepository[IO]], mock[ActionDispatcher[IO]])
}
