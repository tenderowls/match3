val akkaVersion = "2.6.17"
val korolevVersion = "1.1.0"

name := "match3"

val commonSettings = Seq(
  scalacOptions ++= Seq("-Yrangepos", "-deprecation"),
  organization := "com.tenderowls",
  version      := "1.0.0-SNAPSHOT",
  scalaVersion := "2.13.7"
)

lazy val match3 = project
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies += "org.specs2" %% "specs2-core" % "4.8.0" % Test
  )

lazy val server = project
  .settings(commonSettings:_*)
  .settings(
    name := "match3-server",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion
    )
  )
  .dependsOn(match3)

lazy val client = project
  .settings(commonSettings:_*)
  .settings(
    name := "match3-client",
    libraryDependencies ++= Seq(
      "org.fomkin" %% "korolev" % korolevVersion,
    )
  )
  .dependsOn(server)

lazy val standalone = project
  .enablePlugins(UniversalPlugin)
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(DockerPlugin)
  .settings(commonSettings:_*)
  .settings(
    dockerBaseImage := "adoptopenjdk",
    packageName in Docker := "match3",
    version in Docker := "1.1.0",
    maintainer in Docker := "Aleksey Fomkin <aleksey.fomkin@gmail.com>",
    dockerExposedPorts := Seq(8080),
    dockerUsername := Some("fomkin"),
    dockerUpdateLatest := true,
    name := "match3-standalone",
    libraryDependencies ++= Seq(
      "org.fomkin" %% "korolev-akka" % korolevVersion,
      "org.slf4j" % "slf4j-simple" % "1.7.+"
    )
  )
  .dependsOn(client)

lazy val root = project.in(file("."))
  .aggregate(match3, server, client, standalone)
