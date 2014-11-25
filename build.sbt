name := """wbox"""

version := "1.0-SNAPSHOT"

lazy val root = (project in file(".")).enablePlugins(PlayScala).settings(
  sbt.Keys.fork in Test := false
)

scalaVersion := "2.11.1"

resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  ws,
  "org.reactivemongo" %% "reactivemongo" % "0.11.0-SNAPSHOT"
)
