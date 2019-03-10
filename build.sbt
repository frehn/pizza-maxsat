name := "pizza-maxsat"

version := "0.1"

scalaVersion := "2.12.8"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.ow2.sat4j" % "org.ow2.sat4j.maxsat" % "2.3.4"

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

mainClass in (Compile, run) := Some("Main")