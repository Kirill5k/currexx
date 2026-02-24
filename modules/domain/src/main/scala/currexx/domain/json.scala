package currexx.domain

import cats.syntax.either.*
import io.circe.{Decoder, Encoder, Json}
import io.circe.syntax.*

import scala.concurrent.duration.FiniteDuration
import scala.util.Try

object json extends JsonCodecs

transparent trait JsonCodecs {
  inline given Encoder[FiniteDuration] = Encoder.encodeString.contramap(_.toCoarsest.toString)
  inline given Decoder[FiniteDuration] = Decoder.decodeString.emap { fdStr =>
    Try {
      val Array(length, unit) = fdStr.split(" ")
      FiniteDuration(length.toLong, unit)
    }.toEither.leftMap(_ => s"$fdStr is not valid finite duration string. Expected format is '<length> <unit>'")
  }
}

transparent trait JsonSyntax {
  extension [A](value: A)
    def asJsonWithKind(kind: String)(using encoder: Encoder.AsObject[A]): Json =
      value.asJsonObject.add("kind", Json.fromString(kind)).asJson
}
