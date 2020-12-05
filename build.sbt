name := "scala-persistence"

version := "1.0"

scalaVersion := "2.13.3"

val slf4jVersion = "1.7.5"
val derbyVersion = "10.15.2.0"

libraryDependencies ++= Seq(
  "org.slf4j" % "slf4j-api" % slf4jVersion,
  "org.slf4j" % "slf4j-simple" % slf4jVersion,

  "org.apache.derby" % "derby" % derbyVersion,

  "org.scala-lang" % "scala-reflect" % scalaVersion.value,

  "org.scalatest" %% "scalatest" % "3.0.8" % Test
)

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _ @ _*) => MergeStrategy.discard
  case _ => MergeStrategy.first
}
