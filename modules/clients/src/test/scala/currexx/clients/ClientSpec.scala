package currexx.clients

import cats.effect.IO
import cats.effect.unsafe.IORuntime
import org.scalatest.Assertion
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import sttp.capabilities.WebSockets
import sttp.capabilities.fs2.Fs2Streams
import sttp.client3
import sttp.client3.httpclient.fs2.HttpClientFs2Backend
import sttp.client3.testing.SttpBackendStub
import sttp.model.{Header, Method}

import scala.concurrent.Future
import scala.io.Source

trait ClientSpec extends AsyncWordSpec with Matchers {
  given Logger[IO] = Slf4jLogger.getLogger[IO]

  extension [A](io: IO[A])
    def asserting(f: A => Assertion): Future[Assertion] =
      io.map(f).unsafeToFuture()(IORuntime.global)
    def assertIsVoid: Future[Assertion] = asserting(_ mustBe ())
    def assertError(error: Throwable): Future[Assertion] =
      io.attempt.asserting(_ mustBe Left(error))

  def backendStub: SttpBackendStub[IO, Fs2Streams[IO] with WebSockets] =
    HttpClientFs2Backend.stub[IO]

  def json(path: String): String = Source.fromResource(path).getLines().toList.mkString

  extension (req: client3.Request[?, ?])
    def isPost: Boolean                                 = req.method == Method.POST
    def isGet: Boolean                                  = req.method == Method.GET
    def isPut: Boolean                                  = req.method == Method.PUT
    def hasBearerToken(token: String): Boolean          = req.headers.contains(new Header("Authorization", s"Bearer $token"))
    def hasBody(json: String): Boolean                  = req.body.toString.contains(json)
    def hasHost(host: String): Boolean                  = req.uri.host.contains(host)
    def hasPath(path: String): Boolean                  = req.uri.path == path.split("/").filter(_.nonEmpty).toList
    def bodyContains(body: String): Boolean             = req.body.toString.contains(body)
    def hasParams(params: Map[String, String]): Boolean = req.uri.params.toMap.toSet[(String, String)].subsetOf(params.toSet)
    def isGoingTo(url: String): Boolean = {
      val urlParts = url.split("/")
      hasHost(urlParts.head) && req.uri.path.startsWith(urlParts.tail.filter(_.nonEmpty).toList)
    }
}
