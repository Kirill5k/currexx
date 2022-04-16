package currexx.core

import cats.effect.IO
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AsyncWordSpec
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

trait CatsSpec extends AsyncWordSpec with Matchers with MockitoMatchers:
  inline given logger: Logger[IO] = Slf4jLogger.getLogger[IO]
