package currex.core.auth

import currex.core.auth.jwt.BearerToken
import currex.core.auth.session.Session

trait Authenticator[F[_]]:
  def authenticate(token: BearerToken): F[Session]
