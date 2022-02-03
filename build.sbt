ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.1.0"

lazy val root = (project in file("."))
  .settings(
    name := "broker-management",
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.10",
      "org.scalactic" %% "scalactic" % "3.2.10",
      "org.scalatestplus" %% "scalacheck-1-15" % "3.2.9.0" // for using scalacheck from scalatest
    ).map(_ % "test") ++ Seq(
      "com.typesafe" % "config" % "1.4.0",
      "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.4",
      "com.typesafe.play" %% "play-json" % "2.10.0-RC5",
      "com.lihaoyi" %% "requests" % "0.7.0"
    )
  )
