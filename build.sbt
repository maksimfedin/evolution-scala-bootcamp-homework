scalaVersion := "2.13.3"


name := "homework"
organization := "com.evolutiongaming.bootcamp.homework"
version := "1.0"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test
libraryDependencies += "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test

bulkyThresholdInLines := 10
