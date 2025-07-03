import sbt._
import Keys._
ThisBuild / version := "0.1.6-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

libraryDependencies += "net.sourceforge.owlapi" % "owlapi-distribution" % "5.5.1"
libraryDependencies += "com.typesafe.scala-logging" %% "scala-logging" % "3.9.4"
libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.5.18"
libraryDependencies += "com.typesafe" % "config" % "1.4.3"

lazy val root = (project in file("."))
  .settings(
    name := "krom",
    idePackagePrefix := Some("org.nebularis.krom"),
    mainClass := Some("org.nebularis.krom.main")
  )

assemblyMergeStrategy in assembly := {
    case PathList("META-INF", xs@_*) =>
        (xs map {_.toLowerCase}) match {
            case "services" :: xs =>
                MergeStrategy.filterDistinctLines
            case _ => MergeStrategy.discard
        }
    case x => MergeStrategy.first
}
