organization := "com.tenderowls"

name := "match3"

version := "0.1.0-SNAPSHOT"

scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "3.9.1" % Test
)

scalacOptions in Test ++= Seq("-Yrangepos")
