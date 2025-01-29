val scala3Version = "3.6.3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "json-codecs",
    version := "0.1.0-SNAPSHOT",

    scalaVersion := scala3Version,

    libraryDependencies ++= Seq(
      "org.typelevel" %% "jawn-ast" % "1.3.2",
      "org.scalameta" %% "munit" % "1.0.0" % Test
    )
  )
