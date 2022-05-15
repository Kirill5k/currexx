package currexx.core

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import org.scalatest.{Assertion, EitherValues}

import scala.concurrent.Future

trait CatsSpec extends AsyncWordSpec with Matchers with MockitoMatchers with EitherValues {
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
  
  extension [A](io: IO[A])
    def asserting(f: A => Assertion): Future[Assertion] =
      io.map(f).unsafeToFuture()(IORuntime.global)
}
