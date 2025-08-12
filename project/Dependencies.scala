import sbt.*

object Dependencies {
  object Versions {
    val fs2            = "3.12.0"
    val cats           = "2.13.0"
    val commonScala    = "0.1.26"
    val mongo4cats     = "0.7.13"
    val pureConfig     = "0.17.9"
    val circe          = "0.14.14"
    val circeRefined   = "0.15.1"
    val sttp4          = "4.0.9"
    val logback        = "1.5.18"
    val log4cats       = "2.7.1"
    val bcrypt         = "4.3.0"
    val refined        = "0.11.3"
    val tapir          = "1.11.41"
    val jwt            = "11.0.2"
    val cronUtils      = "9.2.1"
    val taggedAdtCodec = "0.11.0"
    val breeze         = "2.1.0"
  }

  object Libraries {
    val fs2            = "co.fs2"               %% "fs2-core"               % Versions.fs2
    val catsFree       = "org.typelevel"        %% "cats-free"              % Versions.cats
    val bcrypt         = "com.github.t3hnar"    %% "scala-bcrypt"           % Versions.bcrypt
    val jwt            = "com.github.jwt-scala" %% "jwt-circe"              % Versions.jwt
    val cronUtils      = "com.cronutils"         % "cron-utils"             % Versions.cronUtils
    val taggedAdtCodec = "org.latestbit"        %% "circe-tagged-adt-codec" % Versions.taggedAdtCodec
    val breeze         = "org.scalanlp"         %% "breeze"                 % Versions.breeze

    object commonScala {
      val cats       = "io.github.kirill5k" %% "common-cats"        % Versions.commonScala
      val http4s     = "io.github.kirill5k" %% "common-http4s"      % Versions.commonScala
      val syntax     = "io.github.kirill5k" %% "common-syntax"      % Versions.commonScala
      val testHttp4s = "io.github.kirill5k" %% "common-http4s-test" % Versions.commonScala
      val testSttp   = "io.github.kirill5k" %% "common-sttp-test"   % Versions.commonScala
    }

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
      val refined = "io.circe" %% "circe-refined" % Versions.circeRefined
      val parser  = "io.circe" %% "circe-parser"  % Versions.circe
      val all     = Seq(core, generic, refined, parser)
    }

    object refined {
      val core = "eu.timepit" %% "refined" % Versions.refined
      val all  = Seq(core)
    }

    object sttp4 {
      val core        = "com.softwaremill.sttp.client4" %% "core"  % Versions.sttp4
      val circe       = "com.softwaremill.sttp.client4" %% "circe" % Versions.sttp4
      val catsBackend = "com.softwaremill.sttp.client4" %% "fs2"   % Versions.sttp4
      val all         = Seq(core, circe, catsBackend)
    }

    object tapir {
      val core   = "com.softwaremill.sttp.tapir" %% "tapir-core"          % Versions.tapir
      val circe  = "com.softwaremill.sttp.tapir" %% "tapir-json-circe"    % Versions.tapir
      val http4s = "com.softwaremill.sttp.tapir" %% "tapir-http4s-server" % Versions.tapir
      val all    = Seq(core, circe, http4s)
    }
  }

  val domain = Seq(
    Libraries.cronUtils,
    Libraries.taggedAdtCodec,
    Libraries.mongo4cats.core,
    Libraries.mongo4cats.circe,
    Libraries.commonScala.cats,
    Libraries.commonScala.syntax
  ) ++
    Libraries.tapir.all ++
    Libraries.logging.all ++
    Libraries.circe.all ++
    Libraries.refined.all

  val algorithms = Seq(
    Libraries.catsFree,
    Libraries.fs2
  )

  val calculations = Seq(
    Libraries.breeze
  )

  val clients = Libraries.sttp4.all

  val core = Seq(
    Libraries.commonScala.http4s,
    Libraries.pureconfig.core,
    Libraries.jwt,
    Libraries.bcrypt.cross(CrossVersion.for3Use2_13)
  )

  val test = Seq(
    Libraries.commonScala.testHttp4s % Test,
    Libraries.commonScala.testSttp   % Test,
    Libraries.mongo4cats.embedded    % Test
  )

}
