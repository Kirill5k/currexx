package currexx.domain

import mongo4cats.bson.ObjectId
import io.circe.{Decoder, Encoder}

import scala.reflect.ClassTag

object types {

  trait Kinded(private[domain] val kindName: String)

  transparent trait EnumType[E <: Kinded: ClassTag](private val enums: Array[E]):
    inline given Encoder[E] = Encoder[String].contramap(_.kindName)
    inline given Decoder[E] = Decoder[String].emap(from)

    def from(kind: String): Either[String, E] =
      enums.find(_.kindName == kind).toRight(s"Unrecognized kind $kind for enum ${implicitly[ClassTag[E]].runtimeClass}")

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
