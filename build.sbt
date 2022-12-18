ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.2.1"

lazy val root = (project in file("."))
  .settings(
    name := "zio-exercise",
    libraryDependencies ++= Seq(
      "dev.zio" %% "zio" % "2.0.4"
    )
  )
