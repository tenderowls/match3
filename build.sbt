name := "match3"

version := "0.1.0"

scalaVersion := "2.11.6"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2" % "2.3.13" % "test"
)

scalacOptions in Test ++= Seq("-Yrangepos")
