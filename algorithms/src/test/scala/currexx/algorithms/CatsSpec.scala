package currexx.algorithms

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.scalatest.{Assertion, EitherValues}

import scala.concurrent.Future

trait CatsSpec extends AsyncWordSpec with Matchers:
  extension [A](io: IO[A])
    def asserting(f: A => Assertion): Future[Assertion] =
      io.map(f).unsafeToFuture()(IORuntime.global)
