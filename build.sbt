val scala3Version = "3.2.2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "files-cli",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % "2.9.0",
      "org.typelevel" %% "cats-effect" % "3.4.6"
    )
  )
