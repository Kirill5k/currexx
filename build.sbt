import com.typesafe.sbt.packager.docker.*
import org.typelevel.scalacoptions.ScalacOptions
import sbtghactions.JavaSpec

ThisBuild / scalaVersion                        := "3.7.2"
ThisBuild / version                             := scala.sys.process.Process("git rev-parse HEAD").!!.trim.slice(0, 7)
ThisBuild / organization                        := "io.github.kirill5k"
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("24"))
ThisBuild / scalacOptions ++= Seq("-Wunused:all", "-Xmax-inlines:256")
ThisBuild / Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement

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
  dockerBaseImage    := "amazoncorretto:24-alpine",
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

val domain = project
  .in(file("modules/domain"))
  .settings(
    name       := "currexx-domain",
    moduleName := "currexx-domain",
    libraryDependencies ++= Dependencies.domain ++ Dependencies.test,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement
  )

val algorithms = project
  .in(file("modules/algorithms"))
  .settings(
    name       := "currexx-algorithms",
    moduleName := "currexx-algorithms",
    libraryDependencies ++= Dependencies.algorithms ++ Dependencies.test,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement
  )

val calculations = project
  .in(file("modules/calculations"))
  .settings(
    name       := "currexx-calculations",
    moduleName := "currexx-calculations",
    libraryDependencies ++= Dependencies.calculations ++ Dependencies.test,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement
  )

val clients = project
  .in(file("modules/clients"))
  .dependsOn(domain)
  .settings(
    name       := "currexx-clients",
    moduleName := "currexx-clients",
    libraryDependencies ++= Dependencies.clients ++ Dependencies.test,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement
  )

val core = project
  .in(file("modules/core"))
  .enablePlugins(JavaAppPackaging, JavaAgent, DockerPlugin)
  .dependsOn(domain % "compile->compile;test->test", clients, calculations)
  .settings(docker)
  .settings(
    name       := "currexx-core",
    moduleName := "currexx-core",
    libraryDependencies ++= Dependencies.core,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement
  )

val backtest = project
  .in(file("modules/backtest"))
  .dependsOn(core, algorithms)
  .settings(
    name       := "currexx-backtest",
    moduleName := "currexx-backtest",
    libraryDependencies ++= Dependencies.test,
    Test / tpolecatExcludeOptions += ScalacOptions.warnNonUnitStatement,
  )

val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name := "currexx"
  )
  .aggregate(core, algorithms, calculations, domain, clients)
