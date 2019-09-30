name := "pizza-maxsat"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.maxsat" % "2.3.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "commons-io" % "commons-io" % "2.6"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-stream" % "2.5.21",
  "com.typesafe.akka" %% "akka-stream-testkit" % "2.5.21" % Test
)

resolvers += Resolver.bintrayRepo("gapt", "maven")
libraryDependencies += "at.logic.gapt" %% "gapt" % "2.14"

mainClass in(Compile, run) := Some("Main")
