val akkaVersion = "2.5.8"
val korolevVersion = "0.8.1"
val commonSettings = Seq(
  organization := "com.tenderowls",
  version      := "1.0.0",
  scalaVersion := "2.12.4"
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
      "com.typesafe.akka" %% "akka-actor" % akkaVersion,
      "com.typesafe.akka" %% "akka-typed" % akkaVersion
    )
  )
  .dependsOn(match3)

lazy val client = project
  .settings(commonSettings:_*)
  .settings(
    libraryDependencies ++= Seq(
      "com.github.fomkin" %% "korolev-server-akkahttp" % korolevVersion,
      "org.slf4j" % "slf4j-simple" % "1.7.+"
    )
  )
  .dependsOn(server)

lazy val root = project.in(file("."))
  .aggregate(match3, server, client)
