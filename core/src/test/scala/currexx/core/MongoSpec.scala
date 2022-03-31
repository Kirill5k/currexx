package currexx.core

import currexx.core.auth.user.*
import mongo4cats.bson.{Document, ObjectId}
import mongo4cats.embedded.EmbeddedMongo

import java.time.Instant

trait MongoSpec extends EmbeddedMongo with CatsSpec {

  def accDoc(
      id: UserId,
      email: UserEmail,
      password: PasswordHash = PasswordHash("password"),
      registrationDate: Instant = Instant.parse("2021-06-01T00:00:00Z")
  ): Document =
    Document(
      Map(
        "_id"              -> ObjectId(id.value),
        "email"            -> email.value,
        "password"         -> password.value,
        "name"             -> Document.parse("""{"first":"John","last":"Bloggs"}"""),
        "registrationDate" -> registrationDate
      )
    )
}
