package currexx.core.auth.session

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import currexx.core.CatsSpec
import currexx.core.auth.jwt.{BearerToken, JwtEncoder, JwtToken}
import currexx.core.auth.session.db.SessionRepository
import currexx.domain.user.UserId
import currexx.domain.session.*
import currexx.domain.errors.AppError
import currexx.core.fixtures.{Sessions, Users}
import org.mockito.ArgumentMatchers.any
import org.mockito.Mockito.{verify, verifyNoInteractions, when}

import java.time.Instant

class SessionServiceSpec extends CatsSpec {

  "A SessionService" when {

    "authenticate" should {
      val bearerToken = BearerToken("token")
      val jwtToken    = JwtToken(Sessions.sid, Users.uid)

      "return error on token decryption failure" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(jwtEnc.decode(any[BearerToken])).thenReturn(IO.raiseError(AppError.InvalidJwtToken("error")))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          sid <- svc.authenticate(bearerToken)
        yield sid

        result.attempt.unsafeToFuture().map { res =>
          verifyNoInteractions(repo)
          verify(jwtEnc).decode(bearerToken)
          res mustBe Left(AppError.InvalidJwtToken("error"))
        }
      }

      "return error when session not found" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(jwtEnc.decode(any[BearerToken])).thenReturn(IO.pure(jwtToken))
        when(repo.find(any[SessionId])).thenReturn(IO.pure(None))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          sid <- svc.authenticate(bearerToken)
        yield sid

        result.attempt.unsafeToFuture().map { res =>
          verify(jwtEnc).decode(bearerToken)
          verify(repo).find(Sessions.sid)
          res mustBe Left(AppError.SessionDoesNotExist(Sessions.sid))
        }
      }

      "return error when user id in token is different from user id in session" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(jwtEnc.decode(any[BearerToken])).thenReturn(IO.pure(jwtToken))
        when(repo.find(any[SessionId])).thenReturn(IO.pure(Some(Sessions.sess.copy(userId = Users.uid2))))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          sid <- svc.authenticate(bearerToken)
        yield sid

        result.attempt.unsafeToFuture().map { res =>
          verify(jwtEnc).decode(bearerToken)
          verify(repo).find(Sessions.sid)
          res mustBe Left(AppError.SomeoneElsesSession)
        }
      }

      "return error when session has expired" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(jwtEnc.decode(any[BearerToken])).thenReturn(IO.pure(jwtToken))
        when(repo.find(any[SessionId])).thenReturn(IO.pure(Some(Sessions.sess.copy(active = false))))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          sid <- svc.authenticate(bearerToken)
        yield sid

        result.attempt.unsafeToFuture().map { res =>
          verify(jwtEnc).decode(bearerToken)
          verify(repo).find(Sessions.sid)
          res mustBe Left(AppError.ExpiredSession)
        }
      }

      "return session on success" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(jwtEnc.decode(any[BearerToken])).thenReturn(IO.pure(jwtToken))
        when(repo.find(any[SessionId])).thenReturn(IO.pure(Some(Sessions.sess)))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          sid <- svc.authenticate(bearerToken)
        yield sid

        result.unsafeToFuture().map { res =>
          verify(jwtEnc).decode(bearerToken)
          verify(repo).find(Sessions.sid)
          res mustBe Sessions.sess
        }
      }
    }

    "create" should {
      "create new session" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(repo.create(any[CreateSession])).thenReturn(IO.pure(Sessions.sid))
        when(jwtEnc.encode(any[JwtToken])).thenReturn(IO.pure(BearerToken("token")))

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          tok <- svc.create(Sessions.create())
        yield tok

        result.unsafeToFuture().map { res =>
          verify(jwtEnc).encode(JwtToken(Sessions.sid, Users.uid))
          verify(repo).create(Sessions.create())
          res mustBe BearerToken("token")
        }
      }
    }

    "find" should {
      "return existing session" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(repo.find(any[SessionId])).thenReturn(IO.pure(Some(Sessions.sess)))

        val result = for
          svc  <- SessionService.make(jwtEnc, repo)
          sess <- svc.find(Sessions.sid)
        yield sess

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(jwtEnc)
          verify(repo).find(Sessions.sid)
          res mustBe Some(Sessions.sess)
        }
      }
    }

    "unauth" should {
      "unauth session" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(repo.unauth(Sessions.sid)).thenReturn(IO.unit)

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          res <- svc.unauth(Sessions.sid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(jwtEnc)
          verify(repo).unauth(Sessions.sid)
          res mustBe ()
        }
      }
    }

    "invalidate" should {
      "invalidate all sessions" in {
        val jwtEnc = mock[JwtEncoder[IO]]
        val repo   = mock[SessionRepository[IO]]
        when(repo.invalidatedAll(any[UserId])).thenReturn(IO.unit)

        val result = for
          svc <- SessionService.make(jwtEnc, repo)
          res <- svc.invalidateAll(Users.uid)
        yield res

        result.unsafeToFuture().map { res =>
          verifyNoInteractions(jwtEnc)
          verify(repo).invalidatedAll(Users.uid)
          res mustBe ()
        }
      }
    }
  }
}

