scalaVersion := "2.13.3"


name := "homework"
organization := "com.evolutiongaming.bootcamp.homework"
version := "1.0"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"
val circeVersion = "0.13.0"

scalacOptions ++= Seq(
    "-Ymacro-annotations"
)

libraryDependencies ++= Seq(
    "org.typelevel" %% "cats-core" % catsVersion,
    "org.scalatest" %% "scalatest" % "3.2.3" % Test,
    "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test,
    "io.circe" %% "circe-core" % circeVersion,
    "io.circe" %% "circe-generic" % circeVersion,
    "io.circe" %% "circe-generic-extras" % circeVersion,
    "io.circe" %% "circe-optics" % circeVersion,
    "io.circe" %% "circe-parser" % circeVersion,
    "org.scalaj" %% "scalaj-http" % "2.4.2" % Test
)

bulkyThresholdInLines := 10
