import com.typesafe.sbt.packager.docker.*
import org.typelevel.scalacoptions.ScalacOptions
import sbtghactions.JavaSpec

ThisBuild / scalaVersion                        := "3.8.1"
ThisBuild / version                             := scala.sys.process.Process("git rev-parse HEAD").!!.trim.slice(0, 7)
ThisBuild / organization                        := "io.github.kirill5k"
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("25"))
ThisBuild / scalacOptions ++= Seq("-Wunused:all", "-Xmax-inlines:256")

val noPublish = Seq(
  publish         := {},
  publishLocal    := {},
  publishArtifact := false,
  publish / skip  := true
)

val docker = Seq(
  packageName        := moduleName.value,
  version            := version.value,
  maintainer         := "immotional@aol.com",
  dockerBaseImage    := "amazoncorretto:25-alpine",
  dockerUpdateLatest := true,
  dockerUsername     := sys.env.get("DOCKER_USERNAME"),
  dockerRepository   := sys.env.get("DOCKER_REPO_URI"),
  makeBatScripts     := Nil,
  dockerEnvVars ++= Map("VERSION" -> version.value),
  dockerCommands := {
    val commands         = dockerCommands.value
    val (stage0, stage1) = commands.span(_ != DockerStageBreak)
    val (before, after)  = stage1.splitAt(4)
    val installBash      = Cmd("RUN", "apk update && apk upgrade && apk add bash")
    stage0 ++ before ++ List(installBash) ++ after
  }
)

val common = Seq(
  Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
  tpolecatExcludeOptions ++= Set(ScalacOptions.fatalWarnings),
  scalacOptions += "-Werror"
)

val domain = project
  .in(file("modules/domain"))
  .settings(common)
  .settings(
    name       := "currexx-domain",
    moduleName := "currexx-domain",
    libraryDependencies ++= Dependencies.domain ++ Dependencies.test
  )

val algorithms = project
  .in(file("modules/algorithms"))
  .settings(common)
  .settings(
    name       := "currexx-algorithms",
    moduleName := "currexx-algorithms",
    libraryDependencies ++= Dependencies.algorithms ++ Dependencies.test
  )

val calculations = project
  .in(file("modules/calculations"))
  .settings(common)
  .settings(
    name       := "currexx-calculations",
    moduleName := "currexx-calculations",
    libraryDependencies ++= Dependencies.calculations ++ Dependencies.test,
  )

val clients = project
  .in(file("modules/clients"))
  .dependsOn(domain)
  .settings(common)
  .settings(
    name       := "currexx-clients",
    moduleName := "currexx-clients",
    libraryDependencies ++= Dependencies.clients ++ Dependencies.test,
  )

val core = project
  .in(file("modules/core"))
  .enablePlugins(JavaAppPackaging, JavaAgent, DockerPlugin)
  .dependsOn(domain % "compile->compile;test->test", clients, calculations)
  .settings(docker)
  .settings(common)
  .settings(
    name       := "currexx-core",
    moduleName := "currexx-core",
    libraryDependencies ++= Dependencies.core,
  )

val backtest = project
  .in(file("modules/backtest"))
  .dependsOn(core, algorithms)
  .settings(common)
  .settings(
    name       := "currexx-backtest",
    moduleName := "currexx-backtest",
    libraryDependencies ++= Dependencies.test,
    Compile / run / fork := true
  )

val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name := "currexx"
  )
  .aggregate(core, algorithms, calculations, domain, clients)
