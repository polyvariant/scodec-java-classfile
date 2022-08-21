ThisBuild / tlBaseVersion := "0.1"
ThisBuild / organization := "org.polyvariant.scodec-java-classfile"
ThisBuild / organizationName := "Polyvariant"
ThisBuild / startYear := Some(2022)
ThisBuild / licenses := Seq(License.Apache2)
ThisBuild / developers := List(tlGitHubDev("kubukoz", "Jakub Kozłowski"))
ThisBuild / tlSonatypeUseLegacyHost := false

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.16")
)

val Scala3 = "3.1.3"

ThisBuild / scalaVersion := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / tlFatalWarnings := false
ThisBuild / tlFatalWarningsInCi := false

val commonSettings = Seq(
  libraryDependencies ++= compilerPlugins ++ Seq(
    "com.disneystreaming" %%% "weaver-cats" % "0.7.15" % Test,
    "com.disneystreaming" %%% "weaver-discipline" % "0.7.15" % Test,
    "com.disneystreaming" %%% "weaver-scalacheck" % "0.7.15" % Test,
  ),
  testFrameworks += new TestFramework("weaver.framework.CatsEffect"),
)

lazy val core = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-bits" % "1.1.34"
    ),
  )

lazy val codecs = crossProject(JVMPlatform, JSPlatform)
  .crossType(CrossType.Pure)
  .settings(
    commonSettings,
    libraryDependencies ++= Seq(
      "org.scodec" %%% "scodec-core" % "2.2.0"
    ),
  )
  .dependsOn(core)

lazy val examples = crossProject(JVMPlatform)
  .crossType(CrossType.Pure)
  .settings(
    libraryDependencies ++= Seq(
      "co.fs2" %%% "fs2-io" % "3.2.12",
      "com.lihaoyi" %%% "pprint" % "0.7.3",
    ),
    commonSettings,
  )
  .dependsOn(codecs)
  .enablePlugins(NoPublishPlugin)

lazy val root = tlCrossRootProject
  .aggregate(core, examples)
  .settings(
    Compile / doc / sources := Seq()
  )
