import Dependencies._

ThisBuild / scalaVersion := "2.13.12"
ThisBuild / version := "0.1.0-SNAPSHOT"
ThisBuild / organization := "github.com.imcamilo"
ThisBuild / organizationName := "com.imcamilo"

val catsVersion = "2.10.0"

lazy val root = (project in file("."))
  .settings(
    name := "cats-yes-cats",
    libraryDependencies ++= Seq(
      munit % Test,
      "org.typelevel" %% "cats-core" % catsVersion,
    )
  )

