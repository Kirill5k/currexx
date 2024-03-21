package currexx.domain

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import kirill5k.common.cats.test.IOMockitoMatchers
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.{Assertion, EitherValues}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.Future

trait IOWordSpec extends AsyncWordSpec with Matchers with IOMockitoMatchers with EitherValues {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]

  extension [A](io: IO[A])
    def assertVoid: Future[Assertion] = asserting(_ mustBe ())
    def throws(error: Throwable): Future[Assertion] =
      io.attempt.asserting(_ mustBe Left(error))
    def asserting(f: A => Assertion): Future[Assertion] =
      io.map(f).unsafeToFuture()(IORuntime.global)
}
