package currexx.core.auth

import jwt.BearerToken
import currexx.core.auth.session.Session

trait Authenticator[F[_]]:
  def authenticate(token: BearerToken): F[Session]
