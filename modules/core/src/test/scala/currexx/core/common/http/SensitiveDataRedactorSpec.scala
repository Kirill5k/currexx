package currexx.core.common.http

import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SensitiveDataRedactorSpec extends AnyWordSpec with Matchers {

  "A SensitiveDataRedactor" should {

    "redact a single sensitive field" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("""{"email":"foo@bar.com","password":"secret123"}""") mustBe """{"email":"foo@bar.com","password":"***"}"""
    }

    "redact multiple sensitive fields" in {
      val redactor = SensitiveDataRedactor[IO](Set("password", "currentPassword", "newPassword"))
      redactor.redact("""{"currentPassword":"old-secret","newPassword":"new-secret"}""") mustBe """{"currentPassword":"***","newPassword":"***"}"""
    }

    "redact sensitive fields with spaces around colon" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("""{"password" : "secret123"}""") mustBe """{"password":"***"}"""
    }

    "not alter non-sensitive fields" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("""{"email":"foo@bar.com","name":"John"}""") mustBe """{"email":"foo@bar.com","name":"John"}"""
    }

    "handle empty string" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("") mustBe ""
    }

    "redact a sensitive field nested in an object" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("""{"user":{"email":"foo@bar.com","password":"secret123"}}""") mustBe """{"user":{"email":"foo@bar.com","password":"***"}}"""
    }

    "redact all occurrences of a sensitive field" in {
      val redactor = SensitiveDataRedactor[IO](Set("password"))
      redactor.redact("""{"password":"first","other":"value","password":"second"}""") mustBe """{"password":"***","other":"value","password":"***"}"""
    }
  }
}
