package currexx.domain

import mongo4cats.bson.ObjectId
import io.circe.{Decoder, Encoder}

object types {

  transparent trait EnumType[E]:
    inline given Encoder[E] = Encoder[String].contramap(unwrap)
    inline given Decoder[E] = Decoder[String].emap(from)

    def from(value: String): Either[String, E]
    def unwrap(e: E): String

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
