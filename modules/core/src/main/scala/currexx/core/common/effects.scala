package currexx.core.common

import cats.Monad

object effects {

  extension [F[_], A](fa: F[Option[A]])
    def flatMapOption[B](default: F[B])(f: A => F[B])(using M: Monad[F]): F[B] =
      M.flatMap(fa) {
        case Some(a) => f(a)
        case None    => default
      }
}
