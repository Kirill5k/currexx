import com.typesafe.sbt.packager.docker.*
import org.typelevel.scalacoptions.ScalacOptions
import sbtghactions.JavaSpec

ThisBuild / scalaVersion := "3.10.1"
ThisBuild / scalacOptions ++= Seq("-Wunused:all", "-Xmax-inlines:256")
ThisBuild / version                             := scala.sys.process.Process("git rev-parse HEAD").!!.trim.slice(0, 7)
ThisBuild / organization                        := "io.github.kirill5k"
ThisBuild / githubWorkflowPublishTargetBranches := Nil
ThisBuild / githubWorkflowJavaVersions          := Seq(JavaSpec.corretto("27"))
// Embedded MongoDB 7.0.0 supports Ubuntu 22.04; newer runner images can fail package resolution.
ThisBuild / githubWorkflowOSes := Seq("ubuntu-22.04")
githubWorkflowDir              := (LocalRootProject / baseDirectory).value / ".github"

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
  dockerBaseImage    := "amazoncorretto:27-alpine",
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
  Test / fork := true,
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
    libraryDependencies ++= Dependencies.test
  )

val clients = project
  .in(file("modules/clients"))
  .dependsOn(domain)
  .settings(common)
  .settings(
    name       := "currexx-clients",
    moduleName := "currexx-clients",
    libraryDependencies ++= Dependencies.clients ++ Dependencies.test
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
    libraryDependencies ++= Dependencies.core
  )

// Disabling fiber tracing lowered the two-round median from 60.586 s to 49.944 s on the original G1 heap.
val backtestJvmDefaults = "-Xms8g -Xmx16g -Xss2m -XX:+UseG1GC -XX:MaxGCPauseMillis=500 -Dcats.effect.tracing.mode=none"
val backtestJvmOptions  = settingKey[Seq[String]]("Heap, collector and runtime options for the backtest fork")

val backtest = project
  .in(file("modules/backtest"))
  .dependsOn(core, algorithms)
  .settings(common)
  .settings(
    name       := "currexx-backtest",
    moduleName := "currexx-backtest",
    libraryDependencies ++= Dependencies.test,
    Compile / run / fork := true,
    // Use sbt --server when changing BACKTEST_JVM_OPTS so the build loads the calling shell's environment.
    backtestJvmOptions := sys.env.getOrElse("BACKTEST_JVM_OPTS", backtestJvmDefaults).split("\\s+").filter(_.nonEmpty).toSeq,
    Compile / run / javaOptions ++= backtestJvmOptions.value ++ Seq("--sun-misc-unsafe-memory-access=allow", "-Dsun.rmi.log.useOld=true")
  )

val root = project
  .in(file("."))
  .settings(noPublish)
  .settings(
    name := "currexx",
    // Delegate once: sbt 2's aggregated test task otherwise runs core tests twice.
    Test / test / aggregate := false,
    Test / test             := (core / Test / test).evaluated
  )
  .aggregate(core, algorithms, calculations, domain, clients, backtest)
