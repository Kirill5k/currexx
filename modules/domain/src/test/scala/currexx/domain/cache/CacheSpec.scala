package currexx.domain.cache

import cats.effect.IO
import currexx.domain.IOWordSpec
import org.scalatest.Assertion
import org.scalatest.wordspec.AsyncWordSpec

import scala.concurrent.Future
import scala.concurrent.duration.*

class CacheSpec extends IOWordSpec {
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
}
