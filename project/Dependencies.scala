import sbt._

object Dependencies {

  object Versions {
    val mongo4cats = "0.4.7"
    val pureConfig = "0.17.1"
    val circe      = "0.14.1"
    val sttp       = "3.5.1"
    val http4s     = "0.23.10"
    val logback    = "1.2.11"
    val log4cats   = "2.2.0"
    val tapir      = "1.0.0-M5"

    val scalaTest = "3.2.11"
    val mockito   = "3.2.10.0"
  }

  object Libraries {

    object mongo4cats {
      val core     = "io.github.kirill5k" %% "mongo4cats-core"     % Versions.mongo4cats
      val circe    = "io.github.kirill5k" %% "mongo4cats-circe"    % Versions.mongo4cats
      val embedded = "io.github.kirill5k" %% "mongo4cats-embedded" % Versions.mongo4cats
    }

    object pureconfig {
      val core = "com.github.pureconfig" %% "pureconfig-core" % Versions.pureConfig
    }

    object logging {
      val logback  = "ch.qos.logback" % "logback-classic" % Versions.logback
      val log4cats = "org.typelevel" %% "log4cats-slf4j"  % Versions.log4cats

      val all = Seq(log4cats, logback)
    }

    object circe {
      val core    = "io.circe" %% "circe-core"    % Versions.circe
      val generic = "io.circe" %% "circe-generic" % Versions.circe
      val parser  = "io.circe" %% "circe-parser"  % Versions.circe

      val all = Seq(core, generic, parser)
    }

    object sttp {
      val core        = "com.softwaremill.sttp.client3" %% "core"                           % Versions.sttp
      val circe       = "com.softwaremill.sttp.client3" %% "circe"                          % Versions.sttp
      val catsBackend = "com.softwaremill.sttp.client3" %% "async-http-client-backend-cats" % Versions.sttp

      val all = Seq(core, circe, catsBackend)
    }

    object tapir {
      val core   = "com.softwaremill.sttp.tapir" %% "tapir-core"          % Versions.tapir
      val circe  = "com.softwaremill.sttp.tapir" %% "tapir-json-circe"    % Versions.tapir
      val http4s = "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % Versions.tapir

      val all = Seq(core, circe, http4s)
    }

    object http4s {
      val blazeServer = "org.http4s" %% "http4s-blaze-server" % Versions.http4s
    }

    val scalaTest = "org.scalatest"     %% "scalatest"   % Versions.scalaTest
    val mockito   = "org.scalatestplus" %% "mockito-3-4" % Versions.mockito
  }

  val core =
    Seq(
      Libraries.mongo4cats.core,
      Libraries.mongo4cats.circe,
      Libraries.pureconfig.core,
      Libraries.http4s.blazeServer
    ) ++
      Libraries.circe.all ++
      Libraries.tapir.all ++
      Libraries.logging.all ++
      Libraries.sttp.all

  val test = Seq(
    Libraries.scalaTest           % Test,
    Libraries.mockito             % Test,
    Libraries.mongo4cats.embedded % Test
  )

}
