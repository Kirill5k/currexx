package currexx.core.auth

import cats.Monad
import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.applicative.*
import cats.syntax.functor.*
import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString
import currexx.domain.user.*
import currexx.domain.session.{CreateSession, IpAddress, Session}
import currexx.domain.errors.AppError.SomeoneElsesSession
import currexx.domain.validations.*
import currexx.core.auth.jwt.BearerToken
import currexx.core.auth.session.SessionService
import currexx.core.auth.user.UserService
import currexx.core.common.http.{Controller, TapirJson, TapirSchema}
import io.circe.Codec
import io.circe.refined.*
import org.http4s.HttpRoutes
import squants.market.Currency
import sttp.model.StatusCode
import sttp.tapir.*
import sttp.tapir.server.http4s.Http4sServerInterpreter

import java.time.Instant
import java.time.temporal.Temporal

final private class AuthController[F[_]](
    private val userService: UserService[F],
    private val sessionService: SessionService[F]
)(using
    F: Async[F]
) extends Controller[F] {
  import AuthController.*

  private def logout(using authenticator: Authenticator[F]) =
    logoutEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        sessionService
          .unauth(session.id)
          .voidResponse
      }

  private def changePassword(using authenticator: Authenticator[F]) =
    changePasswordEndpoint.withAuthenticatedSession
      .serverLogic { session => (uid, req) =>
        F.ensure(uid.pure[F])(SomeoneElsesSession)(_ == session.userId) >>
          userService.changePassword(req.toDomain(uid)) >>
          sessionService.invalidateAll(uid).voidResponse
      }

  private def getCurrentUser(using authenticator: Authenticator[F]) =
    getCurrentUserEndpoint.withAuthenticatedSession
      .serverLogic { session => _ =>
        userService
          .find(session.userId)
          .mapResponse(UserView.from)
      }

  private def createUser =
    createUserEndpoint
      .serverLogic { req =>
        userService
          .create(req.userDetails, req.userPassword)
          .mapResponse(uid => CreateUserResponse(uid.value))
      }

  private def login =
    loginEndpoint
      .serverLogic { (ip, login) =>
        for {
          acc  <- userService.login(login.toDomain)
          time <- F.realTimeInstant
          res  <- sessionService.create(CreateSession(acc.id, ip, time)).mapResponse(LoginResponse.bearer)
        } yield res
      }

  def routes(using authenticator: Authenticator[F]): HttpRoutes[F] =
    Http4sServerInterpreter[F](Controller.serverOptions).toRoutes(
      List(
        login,
        createUser,
        getCurrentUser,
        changePassword,
        logout
      )
    )
}

object AuthController extends TapirSchema with TapirJson {

  final case class CreateUserRequest(
      email: EmailString,
      firstName: NonEmptyString,
      lastName: NonEmptyString,
      password: NonEmptyString
  ) derives Codec.AsObject {
    def userDetails: UserDetails =
      UserDetails(UserEmail.from(email), UserName(firstName.value, lastName.value))
    def userPassword: Password = Password(password.value)
  }

  final case class CreateUserResponse(id: String) derives Codec.AsObject

  final case class LoginRequest(
      email: EmailString,
      password: NonEmptyString
  ) derives Codec.AsObject {
    def toDomain: Login = Login(UserEmail.from(email), Password(password.value))
  }

  final case class LoginResponse(
      access_token: String,
      token_type: String
  ) derives Codec.AsObject

  object LoginResponse {
    def bearer(bearerToken: BearerToken): LoginResponse =
      LoginResponse(access_token = bearerToken.value, token_type = "Bearer")
  }

  final case class UserView(
      id: String,
      firstName: String,
      lastName: String,
      email: String,
      registrationDate: Instant
  ) derives Codec.AsObject

  object UserView {
    def from(acc: User): UserView =
      UserView(
        acc.id.value,
        acc.name.first,
        acc.name.last,
        acc.email.value,
        acc.registrationDate
      )
  }

  final case class ChangePasswordRequest(
      currentPassword: NonEmptyString,
      newPassword: NonEmptyString
  ) derives Codec.AsObject {
    def toDomain(id: UserId): ChangePassword =
      ChangePassword(id, Password(currentPassword.value), Password(newPassword.value))
  }

  private val basePath   = "auth"
  private val userPath   = basePath / "user"
  private val userIdPath = userPath / path[String].validate(Controller.validId).map((s: String) => UserId(s))(_.value).name("user-id")

  val createUserEndpoint = Controller.publicEndpoint.post
    .in(userPath)
    .in(jsonBody[CreateUserRequest])
    .out(statusCode(StatusCode.Created).and(jsonBody[CreateUserResponse]))
    .description("Register new user")

  val loginEndpoint = Controller.publicEndpoint.post
    .in(basePath / "login")
    .in(extractFromRequest(_.connectionInfo.remote.map(ip => IpAddress(ip.getHostName, ip.getPort))))
    .in(jsonBody[LoginRequest])
    .out(jsonBody[LoginResponse])
    .description("Login with the existing user account")

  val getCurrentUserEndpoint = Controller.securedEndpoint.get
    .in(userPath)
    .out(jsonBody[UserView])
    .description("Get currently logged in user")

  val changePasswordEndpoint = Controller.securedEndpoint.post
    .in(userIdPath / "password")
    .in(jsonBody[ChangePasswordRequest])
    .out(statusCode(StatusCode.NoContent))
    .description("Change user's password")

  val logoutEndpoint = Controller.securedEndpoint.post
    .in(basePath / "logout")
    .out(statusCode(StatusCode.NoContent))
    .description("Logout and invalidate current session")

  def make[F[_]: Async](
      userService: UserService[F],
      sessionService: SessionService[F]
  ): F[Controller[F]] =
    Monad[F].pure(AuthController[F](userService, sessionService))
}
