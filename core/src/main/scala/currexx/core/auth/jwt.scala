package currexx.core.auth

import cats.effect.Sync
import cats.syntax.either.*
import currexx.core.common.config.JwtConfig
import currexx.core.common.errors.AppError
import currexx.core.common.types.StringType
import currexx.core.auth.session.SessionId
import currexx.core.auth.user.UserId
import currexx.core.common.config.JwtConfig
import currexx.core.common.errors.AppError
import io.circe.*
import io.circe.syntax.*
import io.circe.Codec
import pdi.jwt.algorithms.{JwtAsymmetricAlgorithm, JwtHmacAlgorithm}
import pdi.jwt.{JwtAlgorithm, JwtCirce}

import scala.util.Failure

object jwt {

  final case class JwtToken(sessionId: SessionId, userId: UserId) derives Codec.AsObject

  opaque type BearerToken = String
  object BearerToken extends StringType[BearerToken]

  trait JwtEncoder[F[_]]:
    def encode(token: JwtToken): F[BearerToken]
    def decode(token: BearerToken): F[JwtToken]

  final private class CirceJwtEncoder[F[_]](
      private val secret: String,
      private val alg: JwtAlgorithm
  )(using
      F: Sync[F]
  ) extends JwtEncoder[F] {

    private val decodeFunc = alg match
      case a if JwtAlgorithm.allHmac().contains(a) =>
        (t: String) => JwtCirce.decodeJson(t, secret, List(a.asInstanceOf[JwtHmacAlgorithm]))
      case a if JwtAlgorithm.allAsymmetric().contains(a) =>
        (t: String) => JwtCirce.decodeJson(t, secret, List(a.asInstanceOf[JwtAsymmetricAlgorithm]))
      case a =>
        (_: String) => Failure(AppError.InvalidJwtEncryptionAlgorithm(a))

    override def encode(token: JwtToken): F[BearerToken] =
      F.delay(JwtCirce.encode(token.asJson, secret, alg))

    override def decode(token: BearerToken): F[JwtToken] =
      F.fromEither(decodeFunc(token).toEither.flatMap(_.as[JwtToken]).leftMap(e => AppError.InvalidJwtToken(e.getMessage)))
  }

  object JwtEncoder:
    def circeJwtEncoder[F[_]](config: JwtConfig)(using F: Sync[F]): F[JwtEncoder[F]] =
      JwtAlgorithm.fromString(config.alg.toUpperCase) match
        case alg if JwtAlgorithm.allHmac().contains(alg) | JwtAlgorithm.allAsymmetric().contains(alg) =>
          F.pure(new CirceJwtEncoder(config.secret, alg))
        case alg =>
          F.raiseError(AppError.InvalidJwtEncryptionAlgorithm(alg))

}
