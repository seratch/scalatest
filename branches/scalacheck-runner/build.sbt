name := "scalatest-scalacheck-runner"
 
version := "0.2.0"
 
scalaVersion := "2.9.0"

parallelExecution in Test := false

libraryDependencies ++= Seq(
  "org.scalatest" % "scalatest-finders_2.9.0" % "0.9.2-SNAPSHOT", 
  "org.scalatest" % "scalatest_2.9.0" % "2.0.M2", 
  "org.scala-tools.testing" % "scalacheck_2.9.0" % "1.9"
)

resolvers += "Sonatype Maven Repository" at "http://oss.sonatype.org/content/repositories/snapshots"