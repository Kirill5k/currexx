import sbt._

object Dependencies {
  object Versions {
    val fs2            = "3.2.13"
    val cats           = "2.8.0"
    val mongo4cats     = "0.6.1"
    val pureConfig     = "0.17.1"
    val circe          = "0.14.2"
    val sttp           = "3.7.6"
    val http4s         = "0.23.12"
    val logback        = "1.4.0"
    val log4cats       = "2.4.0"
    val bcrypt         = "4.3.0"
    val refined        = "0.10.1"
    val tapir          = "1.0.6"
    val jwt            = "9.1.1"
    val cronUtils      = "9.2.0"
    val taggedAdtCodec = "0.10.1"

    val scalaTest = "3.2.13"
    val mockito   = "3.2.10.0"
  }

  object Libraries {
    val fs2            = "co.fs2"               %% "fs2-core"               % Versions.fs2
    val catsFree       = "org.typelevel"        %% "cats-free"              % Versions.cats
    val bcrypt         = "com.github.t3hnar"    %% "scala-bcrypt"           % Versions.bcrypt
    val jwt            = "com.github.jwt-scala" %% "jwt-circe"              % Versions.jwt
    val cronUtils      = "com.cronutils"         % "cron-utils"             % Versions.cronUtils
    val taggedAdtCodec = "org.latestbit"        %% "circe-tagged-adt-codec" % Versions.taggedAdtCodec

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
      val all      = Seq(log4cats, logback)
    }

    object circe {
      val core    = "io.circe" %% "circe-core"    % Versions.circe
      val generic = "io.circe" %% "circe-generic" % Versions.circe
      val refined = "io.circe" %% "circe-refined" % Versions.circe
      val parser  = "io.circe" %% "circe-parser"  % Versions.circe
      val all     = Seq(core, generic, refined, parser)
    }

    object refined {
      val core = "eu.timepit" %% "refined" % Versions.refined
      val all  = Seq(core)
    }

    object sttp {
      val core       = "com.softwaremill.sttp.client3" %% "core"                          % Versions.sttp
      val circe      = "com.softwaremill.sttp.client3" %% "circe"                         % Versions.sttp
      val fs2Backend = "com.softwaremill.sttp.client3" %% "async-http-client-backend-fs2" % Versions.sttp
      val all        = Seq(core, circe, fs2Backend)
    }

    object tapir {
      val core   = "com.softwaremill.sttp.tapir" %% "tapir-core"          % Versions.tapir
      val circe  = "com.softwaremill.sttp.tapir" %% "tapir-json-circe"    % Versions.tapir
      val http4s = "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % Versions.tapir
      val all    = Seq(core, circe, http4s)
    }

    object http4s {
      val blaze = "org.http4s" %% "http4s-blaze-server" % Versions.http4s
    }

    val scalaTest = "org.scalatest"     %% "scalatest"   % Versions.scalaTest
    val mockito   = "org.scalatestplus" %% "mockito-3-4" % Versions.mockito
  }

  val domain = Seq(
    Libraries.cronUtils,
    Libraries.taggedAdtCodec,
    Libraries.mongo4cats.core,
    Libraries.mongo4cats.circe
  ) ++
    Libraries.circe.all ++
    Libraries.refined.all

  val algorithms = Seq(
    Libraries.catsFree,
    Libraries.fs2
  )

  val clients = Libraries.sttp.all ++
    Libraries.logging.all

  val core = Seq(
    Libraries.pureconfig.core,
    Libraries.http4s.blaze,
    Libraries.jwt,
    Libraries.bcrypt.cross(CrossVersion.for3Use2_13)
  ) ++
    Libraries.logging.all ++
    Libraries.tapir.all

  val test = Seq(
    Libraries.scalaTest           % Test,
    Libraries.mockito             % Test,
    Libraries.mongo4cats.embedded % Test
  )

}
