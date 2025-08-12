package currexx.domain

import mongo4cats.bson.ObjectId
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import sttp.tapir.{Schema, Validator}

import scala.reflect.ClassTag

object types {

  object EnumType:
    def printKebabCase[E](e: E): String = e.toString.replaceAll("(?<=[a-z])(?=[A-Z])", "-").toLowerCase
    def printLowerCase[E](e: E): String = e.toString.toLowerCase

  transparent trait EnumType[E: ClassTag](private val enums: () => Array[E], private val unwrap: E => String = EnumType.printKebabCase(_)):
    given Schema[E]     = Schema.string.validate(Validator.enumeration(enums().toList, e => Some(unwrap(e))))
    given Encoder[E]    = Encoder[String].contramap(unwrap(_))
    given Decoder[E]    = Decoder[String].emap(from)
    given KeyEncoder[E] = (e: E) => unwrap(e)
    given KeyDecoder[E] = (key: String) => from(key).toOption

    def from(kind: String): Either[String, E] =
      enums()
        .find(unwrap(_) == kind)
        .toRight(
          s"Invalid value $kind for enum ${implicitly[ClassTag[E]].runtimeClass.getSimpleName}, Accepted values: ${enums().map(unwrap).mkString(",")}"
        )

    extension (e: E) def print: String = unwrap(e)

  transparent trait IdType[Id]:
    def apply(id: String): Id   = id.asInstanceOf[Id]
    def apply(id: ObjectId): Id = apply(id.toHexString)

    given Encoder[Id] = Encoder[String].contramap(_.value)
    given Decoder[Id] = Decoder[String].map(apply)

    extension (id: Id)
      def value: String        = id.asInstanceOf[String]
      def toObjectId: ObjectId = ObjectId(value)

  transparent trait StringType[Str]:
    def apply(str: String): Str = str.asInstanceOf[Str]

    given Encoder[Str] = Encoder[String].contramap(_.value)
    given Decoder[Str] = Decoder[String].map(apply)

    extension (str: Str) def value: String = str.asInstanceOf[String]
}
