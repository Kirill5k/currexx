package currexx.core.common.http

import cats.effect.Sync
import org.typelevel.log4cats.slf4j.Slf4jLogger

final class SensitiveDataRedactor[F[_]: Sync](
    val sensitiveFields: Set[String]
) {
  private val logger  = Slf4jLogger.getLogger[F]
  private val pattern = s"(${sensitiveFields.mkString("|")})".r.pattern.pattern
  private val regex   = s""""($pattern)"\\s*:\\s*"[^"]*"""".r

  private def redact(msg: String): String = regex.replaceAllIn(msg, """"$1":"***"""")

  def log(msg: String): F[Unit] = logger.info(redact(msg))
}

object SensitiveDataRedactor:
  def apply[F[_]: Sync](sensitiveFields: Set[String]): SensitiveDataRedactor[F] =
    new SensitiveDataRedactor[F](sensitiveFields)
