package currexx.core.monitor

import cats.effect.IO
import currexx.clients.data.MarketDataClient
import currexx.core.CatsSpec
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Monitors, Users}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant
import scala.concurrent.duration.*

class MonitorServiceSpec extends CatsSpec {

  "A MonitorService" when {
    "update" should {
      "update monitor properties in the db" in {
        val (repo, disp, client) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.update(Monitors.monitor)
        yield res

        result.asserting { res =>
          verifyNoInteractions(client, disp)
          verify(repo).update(Monitors.monitor)
          res mustBe ()
        }
      }

      "close open orders when currency pair has changed" in {
        val (repo, disp, client) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val updated = Monitors.monitor.copy(currencyPair = Markets.gbpusd)
        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.update(updated)
        yield res

        result.asserting { res =>
          verifyNoInteractions(client)
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          verify(repo).update(updated)
          res mustBe ()
        }
      }

      "close open orders when monitor has been deactivated" in {
        val (repo, disp, client) = mocks
        when(repo.update(any[Monitor])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val updated = Monitors.monitor.copy(active = false)
        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.update(updated)
        yield res

        result.asserting { res =>
          verifyNoInteractions(client)
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          verify(repo).update(updated)
          res mustBe ()
        }
      }
    }

    "resume" should {
      "set active to true" in {
        val (repo, disp, client) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.resume(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(client, disp)
          verify(repo).activate(Users.uid, Monitors.mid, true)
          res mustBe ()
        }
      }
    }

    "pause" should {
      "set active to false" in {
        val (repo, disp, client) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.pure(Monitors.monitor))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.pause(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verifyNoInteractions(client)
          verify(repo).activate(Users.uid, Monitors.mid, false)
          verify(disp).dispatch(Action.CloseOpenOrders(Users.uid, Markets.gbpeur))
          res mustBe ()
        }
      }
    }

    "create" should {
      "store monitor in db and submit query monitor action when schedule is periodic" in {
        val (repo, disp, client) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          mid <- svc.create(Monitors.create())
        yield mid

        result.asserting { mid =>
          verifyNoInteractions(client)
          verify(repo).create(Monitors.create())
          verify(disp).dispatch(Action.QueryMonitor(Users.uid, Monitors.mid))
          mid mustBe Monitors.mid
        }
      }

      "store monitor in db and reschedule when schedule is cron" in {
        val (repo, disp, client) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val schedule = Schedule.Cron("0 7,20 * * 1-5").value
        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          mid <- svc.create(Monitors.create(schedule = schedule))
        yield mid

        result.asserting { mid =>
          verifyNoInteractions(client)
          verify(repo).create(Monitors.create(schedule = schedule))
          verify(disp).dispatch(any[Action.ScheduleMonitor])
          mid mustBe Monitors.mid
        }
      }
    }

    "rescheduleAll" should {
      "schedule monitors without lastQueriedAt to start immediately" in {
        val (repo, disp, client) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(lastQueriedAt = None)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(Action.QueryMonitor(Users.uid, Monitors.mid))
          verifyNoInteractions(client)
          res mustBe ()
        }
      }

      "schedule monitors with old lastQueriedAt to start immediately" in {
        val (repo, disp, client) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(lastQueriedAt = Some(Instant.parse("2020-01-01T00:00:00Z")))))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(Action.QueryMonitor(Users.uid, Monitors.mid))
          verifyNoInteractions(client)
          res mustBe ()
        }
      }

      "schedule monitors with recent lastQueriedAt to wait" in {
        val (repo, disp, client) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.monitor.copy(lastQueriedAt = Some(Instant.now.minusSeconds(50)))))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          verify(disp).dispatch(Action.ScheduleMonitor(Users.uid, Monitors.mid, 10800000.milliseconds))
          verifyNoInteractions(client)
          res mustBe ()
        }
      }
    }

    "query" should {
      "get market data for active monitor" in {
        val (repo, disp, client) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor))
        when(repo.updateQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)
        when(client.timeSeriesData(any[CurrencyPair], any[Interval])).thenReturn(IO.pure(Markets.timeSeriesData))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.query(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(client).timeSeriesData(Markets.gbpeur, Interval.H1)
          verify(disp).dispatch(Action.ProcessMarketData(Users.uid, Markets.timeSeriesData))
          verify(repo).updateQueriedTimestamp(Users.uid, Monitors.mid)
          verify(disp).dispatch(Action.ScheduleMonitor(Users.uid, Monitors.mid, Monitors.period))
          verifyNoMoreInteractions(repo, client, disp)
          res mustBe ()
        }
      }

      "not get market data for inactive monitor" in {
        val (repo, disp, client) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor.copy(active = false)))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.query(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(disp).dispatch(Action.ScheduleMonitor(Users.uid, Monitors.mid, Monitors.period))
          verifyNoInteractions(client)
          verifyNoMoreInteractions(repo, disp)
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MonitorRepository[IO], ActionDispatcher[IO], MarketDataClient[IO]) =
    (mock[MonitorRepository[IO]], mock[ActionDispatcher[IO]], mock[MarketDataClient[IO]])
}
