ThisBuild / organization := "com.collinarnett"
ThisBuild / scalaVersion := "3.2.1"
ThisBuild / version := "0.1"

lazy val slox = (project in file("."))
  .settings(
    name := "slox",
    scalacOptions ++= Seq("-deprecation")
  )
