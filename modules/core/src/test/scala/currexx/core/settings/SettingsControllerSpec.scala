package currexx.core.settings

import cats.effect.IO
import currexx.core.auth.Authenticator
import currexx.core.fixtures.{Sessions, Users}
import currexx.domain.user.UserId
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}

class SettingsControllerSpec extends HttpRoutesWordSpec {

  "A SettingsController" when {
    "GET /settings" should {
      "return global settings" in {
        val svc = mock[SettingsService[IO]]
        when(svc.get(any[UserId])).thenReturnIO(GlobalSettings(userId = Users.uid, signal = None, trade = None, note = None))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.GET, uri"/settings").withAuthHeader()
        val res = SettingsController.make[IO](svc).flatMap(_.routes.orNotFound.run(req))

        val responseBody =
          s"""{
             |  "signal" : null,
             |  "trade" : null,
             |  "note" : null
             |}""".stripMargin

        res mustHaveStatus (Status.Ok, Some(responseBody))
        verify(svc).get(Users.uid)
      }
    }
  }
}
