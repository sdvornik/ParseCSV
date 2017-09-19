
name := "Test"

version := "0.1"

scalaVersion := "2.12.1"

scalaOrganization := "org.typelevel"

libraryDependencies := Seq(
  "com.typesafe.akka" %% "akka-http-core" % "10.0.10",
  "org.typelevel" %% "cats-core" % "1.0.0-MF",
  "org.typelevel" %% "kittens" % "1.0.0-RC0",

  "org.scalatest" %% "scalatest" % "3.2.0-SNAP9" % Test,
  "org.scalacheck" %% "scalacheck" % "1.13.5" % Test,
  "junit" % "junit" % "4.12" % Test
)

scalacOptions ++= Seq(
  "-deprecation",
  "-encoding", "UTF-8",
  "-feature",
  "-language:_"
)

        