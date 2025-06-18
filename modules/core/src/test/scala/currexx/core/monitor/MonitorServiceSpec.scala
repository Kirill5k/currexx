package currexx.core.monitor

import cats.data.NonEmptyList
import cats.effect.IO
import currexx.core.MockActionDispatcher
import currexx.core.common.action.Action
import currexx.core.fixtures.{Markets, Monitors, Users}
import currexx.core.monitor.db.MonitorRepository
import currexx.domain.market.Interval
import currexx.domain.monitor.Schedule
import currexx.domain.user.UserId
import fs2.Stream
import kirill5k.common.cats.Clock
import kirill5k.common.cats.test.IOWordSpec

import java.time.Instant
import scala.concurrent.duration.*

class MonitorServiceSpec extends IOWordSpec {

  "A MonitorService" when {
    val now         = Monitors.queriedAt.plusSeconds(1.hour.toSeconds)
    given Clock[IO] = Clock.mock[IO](now)

    "update" should {
      "update monitor properties in the db" in {
        val (repo, disp) = mocks
        when(repo.update(any[Monitor])).thenReturnIO(Monitors.marketData)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.update(Monitors.marketData)
        yield res

        result.asserting { res =>
          verify(repo).update(Monitors.marketData)
          disp.submittedActions mustBe empty
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
          verify(repo).activate(Users.uid, Monitors.mid, true)
          disp.submittedActions mustBe empty
          res mustBe ()
        }
      }
    }

    "pause" should {
      "set active to false" in {
        val (repo, disp) = mocks
        when(repo.activate(any[UserId], any[MonitorId], any[Boolean])).thenReturnIO(Monitors.marketData)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.pause(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).activate(Users.uid, Monitors.mid, false)
          res mustBe ()
        }
      }
    }

    "create" should {
      "store monitor in db and submit query monitor action when schedule is periodic" in {
        val (repo, disp) = mocks
        when(repo.create(any[CreateMonitor])).thenReturnIO(Monitors.marketData.copy(lastQueriedAt = None))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          mid <- svc.create(Monitors.createMarketData())
        yield mid

        result.asserting { mid =>
          verify(repo).create(Monitors.createMarketData())
          disp.submittedActions mustBe List(Action.ScheduleMonitor(Users.uid, Monitors.mid, 0.seconds))
          mid mustBe Monitors.mid
        }
      }

      "store monitor in db and reschedule when schedule is cron" in {
        val schedule     = Schedule.Cron("0 7,20 * * 1-5").value
        val (repo, disp) = mocks
        when(repo.create(any[CreateMonitor])).thenReturnIO(Monitors.marketData.copy(schedule = schedule))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          mid <- svc.create(Monitors.createMarketData(schedule = schedule))
        yield mid

        result.asserting { mid =>
          verify(repo).create(Monitors.createMarketData(schedule = schedule))
          disp.submittedActions must have size 1
          disp.submittedActions.head mustBe an[Action.ScheduleMonitor]
          mid mustBe Monitors.mid
        }
      }
    }

    "rescheduleAll" should {
      "schedule monitors without lastQueriedAt to start immediately" in {
        val (repo, disp) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.marketData.copy(lastQueriedAt = None)))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          disp.submittedActions mustBe List(Action.ScheduleMonitor(Users.uid, Monitors.mid, 0.seconds))
          res mustBe ()
        }
      }

      "schedule monitors with old lastQueriedAt to start immediately" in {
        val (repo, disp) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.marketData.copy(lastQueriedAt = Some(Instant.parse("2020-01-01T00:00:00Z")))))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          disp.submittedActions mustBe List(Action.ScheduleMonitor(Users.uid, Monitors.mid, 0.seconds))
          res mustBe ()
        }
      }

      "schedule monitors with recent lastQueriedAt to wait" in {
        val (repo, disp) = mocks
        when(repo.stream).thenReturn(Stream(Monitors.marketData))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.rescheduleAll
        yield res

        result.asserting { res =>
          verify(repo).stream
          disp.submittedActions mustBe List(Action.ScheduleMonitor(Users.uid, Monitors.mid, 2.hours))
          res mustBe ()
        }
      }
    }

    "triggerMonitor" should {
      "submit assert profit action for active monitor" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturnIO(Monitors.profit)
        when(repo.updateQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerMonitor(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(repo).updateQueriedTimestamp(Users.uid, Monitors.mid)
          verifyNoMoreInteractions(repo)
          disp.submittedActions mustBe List(
            Action.AssertProfit(Users.uid, NonEmptyList.of(Markets.gbpeur), Monitors.profit.limits),
            Action.ScheduleMonitor(Users.uid, Monitors.mid, 2.hours)
          )
          res mustBe ()
        }
      }

      "not reschedule monitor in case of manual query" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturnIO(Monitors.marketData)
        when(repo.updateQueriedTimestamp(any[UserId], any[MonitorId])).thenReturn(IO.unit)

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerMonitor(Users.uid, Monitors.mid, true)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verify(repo).updateQueriedTimestamp(Users.uid, Monitors.mid)
          verifyNoMoreInteractions(repo)
          disp.submittedActions mustBe List(Action.FetchMarketData(Users.uid, NonEmptyList.of(Markets.gbpeur), Interval.H1))
          res mustBe ()
        }
      }

      "not submit fetch market data and assert profit actions for inactive monitor" in {
        val (repo, disp) = mocks
        when(repo.find(any[UserId], any[MonitorId])).thenReturnIO(Monitors.marketData.copy(active = false))

        val result = for
          svc <- MonitorService.make[IO](repo, disp)
          res <- svc.triggerMonitor(Users.uid, Monitors.mid)
        yield res

        result.asserting { res =>
          verify(repo).find(Users.uid, Monitors.mid)
          verifyNoMoreInteractions(repo)
          disp.submittedActions mustBe List(Action.ScheduleMonitor(Users.uid, Monitors.mid, 2.hours))
          res mustBe ()
        }
      }
    }
  }

  def mocks: (MonitorRepository[IO], MockActionDispatcher[IO]) =
    (mock[MonitorRepository[IO]], MockActionDispatcher.make[IO])
}
