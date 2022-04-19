package currexx.core.monitor

import cats.effect.IO
import currexx.core.ControllerSpec
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Markets, Monitors, Sessions, Users}
import currexx.domain.errors.AppError
import currexx.domain.user.UserId
import org.http4s.implicits.*
import org.http4s.circe.CirceEntityCodec.*
import org.http4s.{Method, Status, Uri}

class MonitorControllerSpec extends ControllerSpec {

  def uriWith(id: MonitorId, suffix: String = "") = Uri.unsafeFromString(s"/monitors/$id$suffix")

  "A MonitorController" when {
    "POST /monitors" should {
      "create new monitor and return 201" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.pure(Monitors.mid))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody = parseJson(
          s"""{
             |"currencyPair": "GBP/EUR",
             |"interval": "H1",
             |"period": "3 hours"
             |}""".stripMargin
        )

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST).withEntity(requestBody)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.Created, Some(s"""{"id":"${Monitors.mid}"}"""))
        verify(svc).create(Monitors.create())
      }

      "return 409 if monitor for the requested currency pair already exists" in {
        val svc = mock[MonitorService[IO]]
        when(svc.create(any[CreateMonitor])).thenReturn(IO.raiseError(AppError.AlreadyBeingMonitored(Markets.gbpeur)))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val requestBody = parseJson(
          s"""{
             |"currencyPair": "GBP/EUR",
             |"interval": "H1",
             |"period": "3 hours"
             |}""".stripMargin
        )

        val req = requestWithAuthHeader(uri"/monitors", method = Method.POST).withEntity(requestBody)
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.Conflict, Some("""{"message":"Monitor for currency pair GBP/EUR already exists"}"""))
        verify(svc).create(Monitors.create())
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
             |"interval": "H1",
             |"period": "3 hours",
             |"lastQueriedAt": "${Monitors.queriedAt}"
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
             |"interval": "H1",
             |"period": "3 hours",
             |"lastQueriedAt": "${Monitors.queriedAt}"
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
             |"interval": "H1",
             |"period": "3 hours",
             |"lastQueriedAt": "${Monitors.queriedAt}"
             |}""".stripMargin

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT).withEntity(parseJson(requestBody))
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
             |"interval": "H1",
             |"period": "3 hours",
             |"lastQueriedAt": "${Monitors.queriedAt}"
             |}""".stripMargin

        val req = requestWithAuthHeader(uriWith(Monitors.mid), method = Method.PUT).withEntity(parseJson(requestBody))
        val res = MonitorController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        verifyJsonResponse(res, Status.BadRequest, Some("""{"message":"The id supplied in the path does not match with the id in the request body"}"""))
        verifyNoInteractions(svc)
      }
    }
  }
}
