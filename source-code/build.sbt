import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "de.codecentric",
      scalaVersion := "2.12.4",
      version      := "0.1.0"
    )),
    name := "free-all-the-things",
    addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.6"),
    scalacOptions ++= Seq("-feature", "-language:higherKinds", "-language:implicitConversions")
  )
