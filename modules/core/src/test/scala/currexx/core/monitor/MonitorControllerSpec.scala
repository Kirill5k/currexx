package currexx.core.monitor

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Markets, Monitors, Sessions, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.{Method, Status, Uri}

class MonitorControllerSpec extends ControllerSpec {

  def uriWith(id: MonitorId, suffix: String = "") = Uri.unsafeFromString(s"/monitors/$id$suffix")

  "A MonitorController" when {
    "POST /monitors" should {
      "create new monitor and return 201" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST)
          .withBody(
            """{
              |"kind": "market-data",
              |"currencyPairs": ["GBP/EUR"],
              |"interval": "H1",
              |"schedule": {"kind":"periodic","period":"3 hours"}
              |}""".stripMargin)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Created, Some(s"""{"id":"${Monitors.mid}"}"""))
        verify(svc).create(Monitors.createMarketData())
      }

      "return 409 if monitor for the requested currency pair already exists" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.raiseError(AppError.AlreadyBeingMonitored(Set(Markets.gbpeur))))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)


        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST)
          .withBody(
            """{
              |"kind": "market-data",
              |"currencyPairs": ["GBP/EUR"],
              |"interval": "H1",
              |"schedule": {"kind":"periodic","period":"3 hours"}
              |}""".stripMargin)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Conflict, Some("""{"message":"Monitor for currency pair GBPEUR already exists"}"""))
        verify(svc).create(Monitors.createMarketData())
      }

      "validation error when profit monitor schedule has missing boundaries" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST)
          .withBody(
            """{
              |"currencyPairs": ["GBP/EUR"],
              |"kind": "profit",
              |"schedule": {"kind":"periodic","period":"3 hours"},
              |"limits": {}
              |}""".stripMargin)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""{"message":"Limits must have at least one of the fields defined"}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }

    "PUT /monitors/:id/pause" should {
      "pause monitor and return 204" in {
        val svc = mock[MonitorService[IO]]
        when(svc.pause(any[UserId], any[MonitorId])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/pause"), method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).pause(Users.uid, Monitors.mid)
      }

      "error when id is invalid" in {
        val svc = mock[MonitorService[IO]]

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors/foo/pause", method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.UnprocessableEntity, Some("""{"message":"Invalid hexadecimal representation of an id: foo"}"""))
        verifyNoInteractions(svc)
      }

      "return 404 error when monitor does not exist" in {
        val svc = mock[MonitorService[IO]]
        when(svc.pause(any[UserId], any[MonitorId])).thenReturn(IO.raiseError(AppError.EntityDoesNotExist("Monitor", Monitors.mid.value)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/pause"), method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NotFound, Some(s"""{"message":"Monitor with id ${Monitors.mid} does not exist"}"""))
        verify(svc).pause(Users.uid, Monitors.mid)
      }
    }

    "PUT /monitors/:id/resume" should {
      "unpause monitor and return 204" in {
        val svc = mock[MonitorService[IO]]
        when(svc.resume(any[UserId], any[MonitorId])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/resume"), method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).resume(Users.uid, Monitors.mid)
      }
    }

    "DELETE /monitors/:id" should {
      "delete existing monitor and return 204" in {
        val svc = mock[MonitorService[IO]]
        when(svc.delete(any[UserId], any[MonitorId])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.DELETE)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).delete(Users.uid, Monitors.mid)
      }
    }

    "POST /monitors/:id/query" should {
      "manually query the monitor" in {
        val svc = mock[MonitorService[IO]]
        when(svc.triggerMonitor(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/query"), method = Method.POST)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).triggerMonitor(Users.uid, Monitors.mid, true)
      }
    }

    "GET /monitors" should {
      "return all monitors" in {
        val svc = mock[MonitorService[IO]]
        when(svc.getAll(any[UserId])).thenReturn(IO.pure(List(Monitors.marketData, Monitors.profit)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors", method = Method.GET)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""[
             |{
             |"id": "${Monitors.mid}",
             |"active": true,
             |"currencyPairs": ["${Markets.gbpeur}"],
             |"schedule": {"kind":"periodic","period":"3 hours"},
             |"lastQueriedAt": "${Monitors.queriedAt}",
             |"interval": "H1",
             |"kind": "market-data"
             |},
             |{
             |"id": "${Monitors.mid}",
             |"active": true,
             |"currencyPairs": ["${Markets.gbpeur}"],
             |"schedule": {"kind":"periodic","period":"3 hours"},
             |"lastQueriedAt": "${Monitors.queriedAt}",
             |"limits": {"min": -10, "max": 150, "cumulativeMin" : null, "cumulativeMax" : null},
             |"kind": "profit"
             |}
             |]""".stripMargin

        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).getAll(Users.uid)
      }
    }

    "GET /monitors/:id" should {
      "find monitor by id" in {
        val svc = mock[MonitorService[IO]]
        when(svc.get(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.marketData))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.GET)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = s"""{
             |"id": "${Monitors.mid}",
             |"kind": "market-data",
             |"active": true,
             |"currencyPairs": ["${Markets.gbpeur}"],
             |"schedule": {"kind":"periodic","period":"3 hours"},
             |"lastQueriedAt": "${Monitors.queriedAt}",
             |"interval": "H1"
             |}""".stripMargin

        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).get(Users.uid, Monitors.mid)
      }
    }

    "PUT /monitors/:id" should {
      "update monitor" in {
        val svc = mock[MonitorService[IO]]
        when(svc.update(any[Monitor])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT)
          .withBody(
            s"""{
               |"id": "${Monitors.mid}",
               |"kind": "market-data",
               |"active": true,
               |"currencyPairs": ["${Markets.gbpeur}"],
               |"schedule": {"kind":"periodic","period":"3 hours"},
               |"lastQueriedAt": "${Monitors.queriedAt}",
               |"interval": "H1"
               |}""".stripMargin)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(svc).update(Monitors.marketData)
      }

      "return error when id in path is different from id in requesst" in {
        val svc = mock[MonitorService[IO]]
        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT)
          .withBody(
            s"""{
               |"id": "foo",
               |"kind": "market-data",
               |"active": true,
               |"currencyPairs": ["${Markets.gbpeur}"],
               |"schedule": {"kind":"periodic","period":"3 hours"},
               |"lastQueriedAt": "${Monitors.queriedAt}",
               |"interval": "H1"
               |}""".stripMargin)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = """{"message":"Id provided in the path does not match with id in the request body"}"""
        res mustHaveStatus (Status.BadRequest, Some(responseBody))
        verifyNoInteractions(svc)
      }
    }
  }
}
