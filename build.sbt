import AssemblyKeys._

assemblySettings

name := "lixr"

organization in ThisBuild := "eu.liderproject"

version := "0.1"

scalaVersion := "2.10.2"

libraryDependencies ++= Seq(
//  "org.scala-lang.modules" %% "scala-xml" % "1.0.2",
  "org.scalatest" %% "scalatest" % "2.1.7" % "test",
  "org.apache.commons" % "commons-lang3" % "3.3.2",
  "com.twitter" %% "util-core" % "6.23.0",
  "org.scala-lang" % "scala-compiler" % "2.10.2",
  "org.scala-lang" % "scala-library" % "2.10.2"
)

mainClass := Some("eu.liderproject.lixr.Main")
