
// The simplest possible sbt build file is just one line:

scalaVersion := "2.13.3"
// That is, to create a valid sbt build, all you've got to do is define the
// version of Scala you'd like your project to use.

// ============================================================================

// Lines like the above defining `scalaVersion` are called "settings". Settings
// are key/value pairs. In the case of `scalaVersion`, the key is "scalaVersion"
// and the value is "2.13.3"

// It's possible to define many kinds of settings, such as:

name := "homework"
organization := "com.evolutiongaming.bootcamp.homework"
version := "1.0"

val catsVersion = "2.2.0"
val scalaTestVersion = "3.1.0.0-RC2"
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.3" % Test
libraryDependencies += "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestVersion % Test

