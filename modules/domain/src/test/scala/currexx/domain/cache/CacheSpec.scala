package currexx.domain.cache

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future
import scala.concurrent.duration.*

class CacheSpec extends AsyncWordSpec with Matchers {
  "A RefbasedCache" should {
    "return non-expired elements" in {
      val result = for
        cache <- Cache.make[IO, String, String](5.seconds, 1.second)
        _     <- cache.put("foo", "bar")
        _     <- IO.sleep(2.seconds)
        res   <- cache.get("foo")
      yield res

      result.asserting(_ mustBe Some("bar"))
    }

    "return empty option when element expires" in {
      val result = for
        cache <- Cache.make[IO, String, String](1.seconds, 1.second)
        _     <- cache.put("foo", "bar")
        _     <- IO.sleep(2.seconds)
        res   <- cache.get("foo")
      yield res

      result.asserting(_ mustBe None)
    }
  }

  extension [A](io: IO[A])
    def asserting(f: A => Assertion): Future[Assertion] =
      io.map(f).unsafeToFuture()(IORuntime.global)
}
