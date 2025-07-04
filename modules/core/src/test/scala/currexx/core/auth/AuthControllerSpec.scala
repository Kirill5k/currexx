package currexx.core.auth

import cats.effect.IO
import kirill5k.common.http4s.test.HttpRoutesWordSpec
import currexx.core.auth.session.SessionService
import currexx.core.auth.user.UserService
import currexx.domain.session.*
import currexx.domain.user.*
import currexx.domain.errors.AppError.{AccountAlreadyExists, InvalidEmailOrPassword, SessionDoesNotExist}
import currexx.core.auth.jwt.BearerToken
import currexx.core.fixtures.{Sessions, Users}
import org.http4s.implicits.*
import org.http4s.{Method, Request, Status, Uri}
import kirill5k.common.cats.Clock

import java.time.Instant

class AuthControllerSpec extends HttpRoutesWordSpec {

  "An AuthController" when {
    val now         = Instant.now
    given Clock[IO] = Clock.mock[IO](now)

    "GET /auth/user" should {
      "return current account" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.find(any[UserId])).thenReturn(IO.pure(Users.user))

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.GET, uri"/auth/user").withAuthHeader()
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        val resBody =
          s"""{
             |"id":"${Users.uid}",
             |"email":"${Users.email}",
             |"firstName":"${Users.details.name.first}",
             |"lastName":"${Users.details.name.last}",
             |"registrationDate": "${Users.regDate}"
             |}""".stripMargin

        res mustHaveStatus (Status.Ok, Some(resBody))
        verify(usrSvc).find(Sessions.sess.userId)
      }
    }

    "POST /auth/user/:id/password" should {
      "return error when id in path is different from id in session" in {
        val (usrSvc, sessSvc) = mocks

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.POST, uri"/auth/user/60e70e87fb134e0c1a271122/password").withAuthHeader()
          .withBody("""{"newPassword":"new-pwd","currentPassword":"curr-pwd"}""")
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Forbidden, Some("""{"message":"The current session belongs to a different user"}"""))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "return 204 when after updating account password" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.changePassword(any[ChangePassword])).thenReturn(IO.unit)
        when(sessSvc.invalidateAll(any[UserId])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.POST, Uri.unsafeFromString(s"/auth/user/${Users.uid}/password"))
          .withAuthHeader()
          .withBody("""{"newPassword":"new-pwd","currentPassword":"curr-pwd"}""")
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(usrSvc).changePassword(ChangePassword(Users.uid, Password("curr-pwd"), Password("new-pwd")))
        verify(sessSvc).invalidateAll(Users.uid)
      }
    }

    "POST /auth/user" should {
      given auth: Authenticator[IO] = _ => IO.raiseError(new RuntimeException("shouldn't reach this"))
      "return bad request if email is already taken" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.create(any[UserDetails], any[Password])).thenReturn(IO.raiseError(AccountAlreadyExists(UserEmail("foo@bar.com"))))

        val req     = Request[IO](uri = uri"/auth/user", method = Method.POST)
          .withBody("""{"email":"foo@bar.com","password":"pwd","firstName":"John","lastName":"Bloggs"}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Conflict, Some("""{"message":"An account with email foo@bar.com already exists"}"""))
        verify(usrSvc).create(
          UserDetails(UserEmail("foo@bar.com"), UserName("John", "Bloggs")),
          Password("pwd")
        )
      }

      "return bad request when invalid request" in {
        val (usrSvc, sessSvc) = mocks

        val req     = Request[IO](uri = uri"/auth/user", method = Method.POST)
          .withBody("""{"email":"foo@bar.com","password":"","firstName":"John","lastName":"Bloggs"}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.UnprocessableEntity, Some("""{"message":"password must not be empty"}"""))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "create new account and return 201" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.create(any[UserDetails], any[Password])).thenReturn(IO.pure(Users.uid))

        val req     = Request[IO](uri = uri"/auth/user", method = Method.POST)
          .withBody("""{"email":"foo@bar.com","password":"pwd","firstName":"John","lastName":"Bloggs"}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Created, Some(s"""{"id":"${Users.uid}"}"""))
        verify(usrSvc).create(
          UserDetails(UserEmail("foo@bar.com"), UserName("John", "Bloggs")),
          Password("pwd")
        )
        verifyNoInteractions(sessSvc)
      }
    }

    "POST /auth/login" should {

      given auth: Authenticator[IO] = _ => IO.raiseError(new RuntimeException("shouldn't reach this"))

      "return 422 on invalid json" in {
        val (usrSvc, sessSvc) = mocks

        val req = Request[IO](uri = uri"/auth/login", method = Method.POST).withBody("""{foo}""")
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        val responseBody = """{"message":"Invalid message body: Could not decode expected \" got 'foo}' (line 1, column 2) json"}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(responseBody))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "return bad req on parsing error" in {
        val (usrSvc, sessSvc) = mocks

        val req     = Request[IO](uri = uri"/auth/login", method = Method.POST)
          .withBody("""{"email":"foo","password":""}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        val resBody = """{"message":"foo is not a valid email, password must not be empty"}"""
        res mustHaveStatus (Status.UnprocessableEntity, Some(resBody))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "return unauthorized when invalid password or email" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.login(any[Login])).thenReturn(IO.raiseError(InvalidEmailOrPassword))

        val req     = Request[IO](uri = uri"/auth/login", method = Method.POST)
          .withBody("""{"email":"foo@bar.com","password":"bar"}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Unauthorized, Some("""{"message":"Invalid email or password"}"""))
        verify(usrSvc).login(Login(UserEmail("foo@bar.com"), Password("bar")))
      }

      "return bearer token on success" in {
        val (usrSvc, sessSvc) = mocks

        when(usrSvc.login(any[Login])).thenReturn(IO.pure(Users.user))
        when(sessSvc.create(any[CreateSession])).thenReturn(IO.pure(BearerToken("token")))

        val req     = Request[IO](uri = uri"/auth/login", method = Method.POST)
          .withBody("""{"email":"foo@bar.com","password":"bar"}""")
        val res     = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Ok, Some(s"""{"access_token":"token","token_type":"Bearer"}"""))
        verify(usrSvc).login(Login(UserEmail("foo@bar.com"), Password("bar")))
        verify(sessSvc).create(CreateSession(Users.uid, None, now))
      }
    }

    "POST /auth/logout" should {
      "return forbidden if auth header is missing" in {
        val (usrSvc, sessSvc) = mocks

        given auth: Authenticator[IO] = _ => IO.raiseError(new RuntimeException("shouldn't reach this"))

        val req = Request[IO](Method.POST, uri"/auth/logout")
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Forbidden, Some("""{"message":"Missing authorization header"}"""))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "return forbidden if session does not exist" in {
        val (usrSvc, sessSvc) = mocks

        given auth: Authenticator[IO] = (_: BearerToken) => IO.raiseError(SessionDoesNotExist(Sessions.sid))

        val req = Request[IO](Method.POST, uri"/auth/logout").withAuthHeader()
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.Forbidden, Some(s"""{"message":"Session with id ${Sessions.sid} does not exist"}"""))
        verifyNoInteractions(usrSvc, sessSvc)
      }

      "delete session on success" in {
        val (usrSvc, sessSvc) = mocks

        when(sessSvc.unauth(any[SessionId])).thenReturn(IO.unit)

        given auth: Authenticator[IO] = _ => IO.pure(Sessions.sess)

        val req = Request[IO](Method.POST, uri"/auth/logout").withAuthHeader()
        val res = AuthController.make[IO](usrSvc, sessSvc).flatMap(_.routes.orNotFound.run(req))

        res mustHaveStatus (Status.NoContent, None)
        verify(sessSvc).unauth(Sessions.sid)
      }
    }

    def mocks: (UserService[IO], SessionService[IO]) =
      (mock[UserService[IO]], mock[SessionService[IO]])
  }
}
