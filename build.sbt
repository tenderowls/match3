val akkaVersion = "2.5.25"
val korolevVersion = "0.13.0"
val commonSettings = Seq(
  organization := "com.tenderowls",
  version      := "1.0.0-SNAPSHOT",
  scalaVersion := "2.12.10"
)

lazy val match3 = project
  .settings(commonSettings:_*)
  .settings(
    scalacOptions in Test ++= Seq("-Yrangepos"),
    libraryDependencies += "org.specs2" %% "specs2-core" % "3.9.1" % Test
  )

lazy val server = project
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-actor-typed" % akkaVersion
    )
  )
  .dependsOn(match3)

lazy val client = project
  .enablePlugins(UniversalPlugin)
  .enablePlugins(AshScriptPlugin)
  .enablePlugins(DockerPlugin)
  .settings(commonSettings:_*)
  .settings(
    packageName in Docker := "match3",
    version in Docker := "1.0.0",
    maintainer in Docker := "Aleksey Fomkin <aleksey.fomkin@gmail.com>",
    dockerExposedPorts := Seq(8080),
    dockerUsername := Some("fomkin"),
    dockerUpdateLatest := true,
    normalizedName := "match3-client",
    libraryDependencies ++= Seq(
      "com.github.fomkin" %% "korolev-server-akkahttp" % korolevVersion,
      "org.slf4j" % "slf4j-simple" % "1.7.+"
    )
  )
  .dependsOn(server)

lazy val root = project.in(file("."))
  .aggregate(match3, server, client)
