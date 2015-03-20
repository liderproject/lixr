import AssemblyKeys._

assemblySettings

name := "lixr"

organization in ThisBuild := "eu.liderproject"

version := "0.1"

scalaVersion := "2.11.2"

libraryDependencies ++= Seq(
  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test",
  "org.apache.commons" % "commons-lang3" % "3.3.2"
)

mainClass := Some("eu.liderproject.lixr.Main")
