name := "scalatest-scalacheck-runner"
 
version := "0.1.0"
 
scalaVersion := "2.9.0"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest-finders_2.9.0" % "0.9.1-SNAPSHOT", 
  "org.scalatest" % "scalatest_2.9.0" % "2.0.0", 
  "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9"
)

resolvers += "Sonatype Maven Repository" at "http://oss.sonatype.org/content/repositories/snapshots"