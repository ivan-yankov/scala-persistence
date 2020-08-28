name := "scala-persistence"

version := "0.1"

scalaVersion := "2.13.3"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % "1.7.5",
  "org.slf4j" % "slf4j-simple" % "1.7.5",
  "org.scalatest" %% "scalatest" % "3.0.8" % Test,
  "org.scala-lang" % "scala-reflect" % scalaVersion.value
)
