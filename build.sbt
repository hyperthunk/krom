ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

libraryDependencies += "io.argonaut" %% "argonaut" % "6.3.10"
libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.5.1"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18"


lazy val root = (project in file("."))
  .settings(
    name := "krom",
    idePackagePrefix := Some("org.nebularis.krom")
  )

