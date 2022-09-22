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

        val requestBody = s"""{
             |"currencyPair": "GBP/EUR",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}},
             |"profit": {"min": "-10","max": "150","schedule": {"kind":"periodic","period":"3 hours"}}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.Created, Some(s"""{"id":"${Monitors.mid}"}"""))
        verify(svc).create(Monitors.create())
      }

      "return 409 if monitor for the requested currency pair already exists" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.raiseError(AppError.AlreadyBeingMonitored(Markets.gbpeur)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody = s"""{
             |"currencyPair": "GBP/EUR",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}},
             |"profit": {"min": "-10","max": "150","schedule": {"kind":"periodic","period":"3 hours"}}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.Conflict, Some("""{"message":"Monitor for currency pair GBPEUR already exists"}"""))
        verify(svc).create(Monitors.create())
      }

      "validation error when profit monitor schedule has missing boundaries" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody =
          s"""{
             |"currencyPair": "GBP/EUR",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}},
             |"profit": {"schedule": {"kind":"periodic","period":"3 hours"}}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(
          res,
          Status.UnprocessableEntity,
          Some(s"""{"message":"Profit monitor schedule needs to have min or max boundary specified"}""")
        )
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

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).pause(Users.uid, Monitors.mid)
      }

      "error when id is invalid" in {
        val svc = mock[MonitorService[IO]]

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors/foo/pause", method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.UnprocessableEntity, Some("""{"message":"Invalid hexadecimal representation of an id: foo"}"""))
        verifyNoInteractions(svc)
      }

      "return 404 error when monitor does not exist" in {
        val svc = mock[MonitorService[IO]]
        when(svc.pause(any[UserId], any[MonitorId])).thenReturn(IO.raiseError(AppError.EntityDoesNotExist("Monitor", Monitors.mid.value)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/pause"), method = Method.PUT)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NotFound, Some(s"""{"message":"Monitor with id ${Monitors.mid} does not exist"}"""))
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

        verifyJsonResponse(res, Status.NoContent, None)
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

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).delete(Users.uid, Monitors.mid)
      }
    }

    "POST /monitors/:id/query" should {
      "manually query the monitor" in {
        val svc = mock[MonitorService[IO]]
        when(svc.triggerPriceMonitor(any[UserId], any[MonitorId], any[Boolean])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid, "/query"), method = Method.POST)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).triggerPriceMonitor(Users.uid, Monitors.mid, true)
      }
    }

    "GET /monitors" should {
      "return all monitors" in {
        val svc = mock[MonitorService[IO]]
        when(svc.getAll(any[UserId])).thenReturn(IO.pure(List(Monitors.monitor)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uri"/monitors", method = Method.GET)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""[{
             |"id": "${Monitors.mid}",
             |"active": true,
             |"currencyPair": "${Markets.gbpeur}",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": {"min": -10,"max": 150,"schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"}
             |}]""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).getAll(Users.uid)
      }
    }

    "GET /monitors/:id" should {
      "find monitor by id" in {
        val svc = mock[MonitorService[IO]]
        when(svc.get(any[UserId], any[MonitorId])).thenReturn(IO.pure(Monitors.monitor))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.GET)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |"id": "${Monitors.mid}",
             |"active": true,
             |"currencyPair": "${Markets.gbpeur}",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": {"min": -10,"max": 150,"schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"}
             |}""".stripMargin
        verifyJsonResponse(res, Status.Ok, Some(responseBody))
        verify(svc).get(Users.uid, Monitors.mid)
      }
    }

    "PUT /monitors/:id" should {
      "update monitor" in {
        val svc = mock[MonitorService[IO]]
        when(svc.update(any[Monitor])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody =
          s"""{
             |"id": "${Monitors.mid}",
             |"active": true,
             |"currencyPair": "${Markets.gbpeur}",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": {"min": "-10","max": "150","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"}
             |}""".stripMargin

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).update(Monitors.monitor)
      }

      "return error when id in path is different from id in requesst" in {
        val svc = mock[MonitorService[IO]]

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody =
          s"""{
             |"id": "foo",
             |"active": true,
             |"currencyPair": "${Markets.gbpeur}",
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": {"min": "-10","max": "150","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"}
             |}""".stripMargin

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(
          res,
          Status.BadRequest,
          Some("""{"message":"The id supplied in the path does not match with the id in the request body"}""")
        )
        verifyNoInteractions(svc)
      }
    }

    "PUT /monitors/compound" should {
      "return an error when trying to update nonexisting monitor" in {
        val svc = mock[MonitorService[IO]]
        when(svc.getAll(any[UserId])).thenReturn(IO.pure(List(Monitors.monitor)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody =
          s"""{
             |"currencyPairs": ["GBPUSD", "EURDKK"],
             |"active": true,
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": {"min": "-10","max": "150","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"}
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/monitors/compound", method = Method.PUT).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NotFound, Some("""{"message":"Currency pairs GBPUSD, EURDKK are not being tracked"}"""))
        verify(svc).getAll(Users.uid)
        verifyNoMoreInteractions(svc)
      }

      "update multiple monitors at once" in {
        val svc = mock[MonitorService[IO]]

        val monitors = List(Monitors.gen(profit = None), Monitors.gen(pair = Markets.gbpusd, profit = None))
        when(svc.getAll(any[UserId])).thenReturn(IO.pure(monitors))
        when(svc.update(any[Monitor])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody =
          s"""{
             |"currencyPairs": ["GBPUSD", "GBPEUR"],
             |"active": true,
             |"price": {"interval": "H1","schedule": {"kind":"periodic","period":"3 hours"}, "lastQueriedAt": "${Monitors.queriedAt}"},
             |"profit": null
             |}""".stripMargin

        val req = requestWithAuthHeader(uri"/monitors/compound", method = Method.PUT).withJsonBody(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.NoContent, None)
        verify(svc).getAll(Users.uid)
        monitors.foreach(m => verify(svc).update(m.copy(profit = None)))
      }
    }
  }
}
