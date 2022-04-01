package currexx.core.auth

import cats.effect.{Async, Temporal}
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.Resources
import currexx.core.auth.user.{PasswordEncryptor, UserService}
import currexx.core.auth.user.db.UserRepository
import currexx.core.auth.session.db.SessionRepository
import currexx.core.auth.session.SessionService
import currexx.core.common.config.AuthConfig
import currexx.core.common.http.Controller
import jwt.JwtEncoder
import org.http4s.HttpRoutes
import org.typelevel.log4cats.Logger

final class Auth[F[_]] private (
    val session: SessionService[F],
    val controller: Controller[F]
)

object Auth:
  def make[F[_]: Async: Logger](config: AuthConfig, resources: Resources[F]): F[Auth[F]] =
    for
      sessRepo <- SessionRepository.make[F](resources.mongo)
      jwtEnc   <- JwtEncoder.circeJwtEncoder[F](config.jwt)
      sessSvc  <- SessionService.make[F](jwtEnc, sessRepo)
      accRepo  <- UserRepository.make[F](resources.mongo)
      encr     <- PasswordEncryptor.make[F](config)
      usrSvc   <- UserService.make[F](accRepo, encr)
      authCtrl <- AuthController.make[F](usrSvc, sessSvc)
    yield Auth[F](sessSvc, authCtrl)
