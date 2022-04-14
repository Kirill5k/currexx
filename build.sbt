import com.typesafe.sbt.packager.docker._
import sbtghactions.JavaSpec

ThisBuild / scalaVersion                        := "3.1.1"
ThisBuild / version                             := scala.sys.process.Process("git rev-parse HEAD").!!.trim.slice(0, 7)
ThisBuild / organization                        := "io.github.kirill5k"
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.temurin("17"))

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
  dockerBaseImage    := "amazoncorretto:17.0.1-alpine",
  dockerUpdateLatest := true,
  dockerUsername     := sys.env.get("DOCKER_USERNAME"),
  dockerRepository   := sys.env.get("DOCKER_REPO_URI"),
  makeBatScripts     := Nil,
  dockerCommands := {
    val commands         = dockerCommands.value
    val (stage0, stage1) = commands.span(_ != DockerStageBreak)
    val (before, after)  = stage1.splitAt(4)
    val installBash      = Cmd("RUN", "apk update && apk upgrade && apk add bash")
    stage0 ++ before ++ List(installBash) ++ after
  }
)

val domain = project
  .in(file("domain"))
  .settings(
    name       := "currexx-domain",
    moduleName := "currexx-domain",
    libraryDependencies ++= Dependencies.domain ++ Dependencies.test
  )

val calculations = project
  .in(file("calculations"))
  .settings(
    name       := "currexx-calculations",
    moduleName := "currexx-calculations",
    libraryDependencies ++= Dependencies.test
  )

val clients = project
  .in(file("clients"))
  .dependsOn(domain % "compile->compile;test->test")
  .settings(
    name       := "currexx-clients",
    moduleName := "currexx-clients",
    libraryDependencies ++= Dependencies.clients ++ Dependencies.test
  )

val core = project
  .in(file("core"))
  .enablePlugins(JavaAppPackaging, JavaAgent, DockerPlugin)
  .dependsOn(domain % "compile->compile;test->test", clients)
  .settings(docker)
  .settings(
    name       := "currexx-core",
    moduleName := "currexx-core",
    libraryDependencies ++= Dependencies.core
  )

val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name := "currexx"
  )
  .aggregate(core, calculations)
