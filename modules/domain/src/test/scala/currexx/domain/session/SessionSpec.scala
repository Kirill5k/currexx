package currexx.domain.session

import io.circe.parser.*
import io.circe.syntax.*
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SessionSpec extends AnyWordSpec with Matchers {

  "A SessionStatus codec" should {
    "encode session status to json" in {
      val statues = """["authenticated","logged-out","invalidated"]"""

      decode[List[SessionStatus]](statues) mustBe Right(SessionStatus.values.toList)
      SessionStatus.values.toList.asJson.noSpaces mustBe statues
    }
  }
}
