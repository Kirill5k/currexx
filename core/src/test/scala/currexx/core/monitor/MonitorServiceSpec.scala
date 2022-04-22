package currexx.core.monitor

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.clients.data.MarketDataClient
import currexx.core.CatsSpec
import currexx.core.common.action.{Action, ActionDispatcher}
import currexx.core.fixtures.{Markets, Monitors, Users}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.market.{CurrencyPair, Interval}
import currexx.domain.user.UserId
import fs2.Stream

import java.time.Instant

class MonitorServiceSpec extends CatsSpec {

  "A MonitorService" when {
    "resume" should {
      "set active to true" in {
        val (repo, disp, client) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.resume(Users.uid, Monitors.mid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(client, disp)
          verify(repo).activate(Users.uid, Monitors.mid, true)
          res mustBe ()
        }
      }
    }

    "pause" should {
      "set active to false" in {
        val (repo, disp, client) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          res <- svc.pause(Users.uid, Monitors.mid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(client, disp)
          verify(repo).activate(Users.uid, Monitors.mid, false)
          res mustBe ()
        }
      }
    }

    "create" should {
      "store monitor in db and submit query monitor action" in {
        val (repo, disp, client) = mocks
        when(repo.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))
        when(disp.dispatch(any[Action])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp, client)
          mid <- svc.create(Monitors.create())
        yield mid

        result.unsafeToFuture().map { mid =>
          verifyNoInteractions(client)
          verify(repo).create(Monitors.create())
          verify(disp).dispatch(Action.QueryMonitor(Users.uid, Monitors.mid))
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

        result.unsafeToFuture().map { res =>
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

        result.unsafeToFuture().map { res =>
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

        result.unsafeToFuture().map { res =>
          verify(repo).stream
          verify(disp).dispatch(any[Action.ScheduleMonitor])
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

        result.unsafeToFuture().map { res =>
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

        result.unsafeToFuture().map { res =>
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
