package currexx.core.auth

import cats.effect.Async
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import currexx.core.auth.user.UserService
import currexx.core.auth.user.db.UserRepository
import currexx.core.auth.session.db.SessionRepository
import currexx.core.auth.session.SessionService
import currexx.core.auth.user.PasswordEncryptor
import currexx.core.common.action.ActionDispatcher
import currexx.core.common.config.AuthConfig
import currexx.core.common.http.Controller
import kirill5k.common.cats.Clock
import jwt.JwtEncoder
import mongo4cats.database.MongoDatabase

final class Auth[F[_]] private (
    val authenticator: Authenticator[F],
    val controller: Controller[F]
)

object Auth:
  def make[F[_]: Async: Clock](config: AuthConfig, database: MongoDatabase[F], disp: ActionDispatcher[F]): F[Auth[F]] =
    for
      sessRepo <- SessionRepository.make[F](database)
      jwtEnc   <- JwtEncoder.circeJwtEncoder[F](config.jwt)
      sessSvc  <- SessionService.make[F](jwtEnc, sessRepo)
      accRepo  <- UserRepository.make[F](database)
      encr     <- PasswordEncryptor.make[F](config)
      usrSvc   <- UserService.make[F](accRepo, encr, disp)
      authCtrl <- AuthController.make[F](usrSvc, sessSvc)
    yield Auth[F](sessSvc.authenticate(_), authCtrl)
