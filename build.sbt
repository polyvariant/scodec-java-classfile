inThisBuild(
  List(
    organization := "org.polyvariant",
    homepage := Some(url("https://github.com/polyvariant/scodec-java-classfile")),
    licenses := List("Apache-2.0" -> url("http://www.apache.org/licenses/LICENSE-2.0")),
    developers := List(
      Developer(
        "kubukoz",
        "Jakub Kozłowski",
        "kubukoz@gmail.com",
        url("https://blog.kubukoz.com"),
      )
    ),
    Compile / doc / sources := Seq(),
    sonatypeCredentialHost := "s01.oss.sonatype.org",
    sonatypeRepository := "https://s01.oss.sonatype.org/service/local",
  )
)

val Scala3 = "3.1.3"

def crossPlugin(x: sbt.librarymanagement.ModuleID) = compilerPlugin(x.cross(CrossVersion.full))

val compilerPlugins = List(
  crossPlugin("org.polyvariant" % "better-tostring" % "0.3.16")
)

ThisBuild / versionScheme := Some("early-semver")

ThisBuild / scalaVersion := Scala3
ThisBuild / crossScalaVersions := Seq(Scala3)

Global / onChangedBuildSource := ReloadOnSourceChanges

ThisBuild / githubWorkflowTargetTags ++= Seq("v*")
ThisBuild / githubWorkflowPublishTargetBranches += RefPredicate.StartsWith(Ref.Tag("v"))
ThisBuild / githubWorkflowPublish := Seq(
  WorkflowStep.Sbt(
    List("ci-release"),
    env =
      List(
        "PGP_PASSPHRASE",
        "PGP_SECRET",
        "SONATYPE_PASSWORD",
        "SONATYPE_USERNAME",
      ).map { envKey =>
        envKey -> s"$${{ secrets.$envKey }}"
      }.toMap,
  )
)

lazy val core = project
  .settings(
    name := "scodec-java-classfile",
    libraryDependencies ++= Seq(
      "org.scodec" %% "scodec-core" % "2.2.0",
      "org.scodec" %% "scodec-cats" % "1.1.0",
      "co.fs2" %% "fs2-io" % "3.2.12",
      "com.lihaoyi" %% "pprint" % "0.7.3",
    ) ++ compilerPlugins,
    scalacOptions -= "-Xfatal-warnings",
  )

lazy val examples = project
  .settings(publish := {})
  .dependsOn(core)

lazy val root = project
  .in(file("."))
  .settings(publish := {})
  .aggregate(core, examples)
